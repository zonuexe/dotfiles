#!/usr/bin/env python3
"""Drive an external coding agent over ACP (Agent Client Protocol) as a one-shot worker.

Speaks JSON-RPC over stdio to an ACP agent: initialize -> session/new -> session/prompt,
auto-approving permission requests so the agent can work unattended.
Writes agent_message.txt, tool_calls.txt, and result.json (incl. the verified model) to --out.

SAFETY: this auto-approves the external agent's edits and shell commands for whatever
directory you pass as --cwd. Skill policy: live tree for ordinary work delegation (inplace);
git worktree / `cp -Rc` clone when verifying that a skill/procedure can be completed by an
external model (isolated), or for discardable multi-model runs.

Usage:
  python acp_run.py --agent opencode --model opencode-go/glm-5.2 \
      --cwd <workspace> --prompt-file <task.txt> --out <out-dir> [--timeout 1200]

Add new agents in the AGENTS registry below (command resolver + model setup + model verify).
"""
import argparse, json, os, shlex, shutil, subprocess, threading, time, traceback


# ──────────────────────────── Agent registry ────────────────────────────
# Each agent provides:
#   command(cwd)         -> argv list to spawn its ACP server (resolve local vs npx, etc.)
#   set_model(cwd,model) -> configure the model for a run in `cwd` (or no-op)
#   verify_model(cwd,sid)-> best-effort: return the model actually used, or None
#
# These are VERIFIED SNAPSHOTS, not invented defaults. To add an agent:
#   1. confirm it's in the ACP directory: https://agentclientprotocol.com/get-started/agents
#   2. get its ACP launch command from its OWN docs (the directory lists agents, not commands)
#   3. verify the command with a live handshake before trusting it; cite the source in a comment.
# OpenCode (below): `opencode acp` per https://opencode.ai/docs/acp/, handshake-verified.

def _opencode_base():
    """Prefer a locally installed `opencode`; otherwise fall back to npx."""
    if shutil.which("opencode"):
        return ["opencode"]
    return ["npx", "-y", "opencode-ai@latest"]

def _opencode_command(cwd):
    return _opencode_base() + ["acp", "--cwd", cwd]

def _opencode_set_model(cwd, model):
    if not model:
        return
    if "/" not in model:                       # accept short ids; default this user's provider
        model = "opencode-go/" + model
    cfg_path = os.path.join(cwd, "opencode.json")
    cfg = {}
    if os.path.exists(cfg_path):
        try:
            cfg = json.loads(open(cfg_path).read())
        except Exception:
            cfg = {}
    cfg.setdefault("$schema", "https://opencode.ai/config.json")
    cfg["model"] = model
    with open(cfg_path, "w") as f:
        json.dump(cfg, f, indent=2)

def _opencode_verify_model(cwd, sid):
    """Read the model actually used from OpenCode's session record.

    `opencode export` can emit a large document; a strict json.loads is brittle (it can fail on
    multi-turn sessions). Regex straight out of the text is robust and sufficient — the model id is
    constant within a session, so the first match is the answer.
    """
    if not sid:
        return None
    try:
        out = subprocess.run(_opencode_base() + ["export", sid], cwd=cwd,
                             capture_output=True, text=True, timeout=120).stdout or ""
    except Exception:
        return None
    import re
    m = re.search(r'"modelID"\s*:\s*"([^"]+)"', out)
    p = re.search(r'"providerID"\s*:\s*"([^"]+)"', out)
    if m:
        return f"{p.group(1)}/{m.group(1)}" if p else m.group(1)
    return None

AGENTS = {
    "opencode": {
        "command": _opencode_command,
        "set_model": _opencode_set_model,
        "verify_model": _opencode_verify_model,
    },
}


# ──────────────────────────── ACP client ────────────────────────────
class ACP:
    def __init__(self, command, cwd, out):
        self.command = command; self.cwd = cwd; self.out = out
        self.proc = None; self._id = 0; self._pending = {}; self._lock = threading.Lock()
        self.transcript = []; self.tool_calls = []
        os.makedirs(out, exist_ok=True)
        self.log = open(os.path.join(out, "acp_transcript.log"), "w")

    def start(self):
        self.proc = subprocess.Popen(
            self.command, cwd=self.cwd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=open(os.path.join(self.out, "acp_stderr.log"), "w"), bufsize=0)
        threading.Thread(target=self._reader, daemon=True).start()

    def _send(self, obj):
        self.log.write(">> " + json.dumps(obj)[:600] + "\n"); self.log.flush()
        self.proc.stdin.write((json.dumps(obj) + "\n").encode()); self.proc.stdin.flush()

    def request(self, method, params, wait=120):
        with self._lock:
            self._id += 1; rid = self._id
            ev = threading.Event(); self._pending[rid] = {"event": ev}
        self._send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        if not ev.wait(wait):
            raise TimeoutError(f"timeout waiting for {method}")
        slot = self._pending.pop(rid)
        if "error" in slot:
            raise RuntimeError(f"{method} error: {slot['error']}")
        return slot.get("result")

    def _respond(self, rid, result=None, error=None):
        msg = {"jsonrpc": "2.0", "id": rid}
        msg["error" if error is not None else "result"] = error if error is not None else result
        self._send(msg)

    def _reader(self):
        dec = json.JSONDecoder(); buf = ""
        try:
            while True:
                chunk = self.proc.stdout.read(4096)
                if not chunk:
                    break
                buf += chunk.decode("utf-8", "replace")
                while buf.strip():
                    buf = buf.lstrip()
                    try:
                        obj, idx = dec.raw_decode(buf)
                    except json.JSONDecodeError:
                        break
                    buf = buf[idx:]; self._handle(obj)
        except Exception:
            self.log.write("READER ERR\n" + traceback.format_exc()); self.log.flush()

    def _handle(self, obj):
        self.log.write("<< " + json.dumps(obj)[:600] + "\n"); self.log.flush()
        if "id" in obj and ("result" in obj or "error" in obj):       # response to us
            slot = self._pending.get(obj["id"])
            if slot:
                slot["error" if "error" in obj else "result"] = obj.get("error", obj.get("result"))
                slot["event"].set()
            return
        if "method" in obj and "id" in obj:                            # request from agent
            self._server_request(obj["id"], obj["method"], obj.get("params") or {})
            return
        if "method" in obj:                                            # notification
            self._notification(obj["method"], obj.get("params") or {})

    def _server_request(self, rid, method, params):
        if method == "session/request_permission":
            opts = params.get("options", [])
            pick = next((o["optionId"] for o in opts
                         if o.get("kind") in ("allow_once", "allow_always") or "allow" in o.get("optionId", "")), None)
            if pick is None and opts:
                pick = opts[0].get("optionId")
            self.tool_calls.append(("permission", json.dumps(params.get("toolCall", {}))[:160]))
            self._respond(rid, {"outcome": {"outcome": "selected", "optionId": pick}})
        elif method == "fs/read_text_file":
            try:
                self._respond(rid, {"content": open(params["path"]).read()})
            except Exception as e:
                self._respond(rid, error={"code": -32000, "message": str(e)})
        elif method == "fs/write_text_file":
            try:
                open(params["path"], "w").write(params.get("content", "")); self._respond(rid, {})
            except Exception as e:
                self._respond(rid, error={"code": -32000, "message": str(e)})
        else:
            self._respond(rid, error={"code": -32601, "message": f"unhandled {method}"})

    def _notification(self, method, params):
        if method == "session/update":
            up = params.get("update", {}); k = up.get("sessionUpdate")
            if k == "agent_message_chunk":
                c = up.get("content", {})
                if c.get("type") == "text":
                    self.transcript.append(c.get("text", ""))
            elif k == "tool_call":
                self.tool_calls.append((up.get("kind", "?"), (up.get("title") or "")[:160]))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--agent", default="opencode", choices=list(AGENTS),
                    help="Registered agent with model setup + verification (default: opencode).")
    ap.add_argument("--cmd", default=None,
                    help="Raw ACP launch command for any agent (overrides --agent), e.g. "
                         "'qwen --acp' or 'gemini --acp' — see references/agents.md. "
                         "Model setup and verification are skipped for raw commands.")
    ap.add_argument("--model", default=None)
    ap.add_argument("--cwd", required=True,
                    help="Working directory for the agent (isolated sandbox or live tree). "
                         "Auto-approves edits/commands in this path.")
    ap.add_argument("--prompt-file", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--timeout", type=int, default=1200)
    a = ap.parse_args()

    cwd = os.path.abspath(a.cwd)
    if a.cmd:                                   # generic: drive any ACP agent by its launch command
        command = shlex.split(a.cmd)
        set_model = lambda c, m: None
        verify_model = lambda c, s: None
        agent_label = a.cmd
    else:                                       # registered agent (model setup + verification)
        agent = AGENTS[a.agent]
        command = agent["command"](cwd)
        set_model, verify_model = agent["set_model"], agent["verify_model"]
        agent_label = a.agent
        set_model(cwd, a.model)
    prompt = open(a.prompt_file).read()
    client = ACP(command, cwd, a.out)
    client.start()
    result = {"ok": False, "agent": agent_label, "model_requested": a.model}
    try:
        init = client.request("initialize", {
            "protocolVersion": 1,
            "clientCapabilities": {"fs": {"readTextFile": True, "writeTextFile": True}},
            "clientInfo": {"name": "acp-agent-runner", "version": "1.0.0"},
        }, wait=90)
        if init.get("authMethods"):
            result["authMethods"] = [m.get("id") for m in init["authMethods"]]
            try:
                client.request("authenticate", {"methodId": init["authMethods"][0]["id"]}, wait=60)
            except Exception as e:
                result["auth_note"] = str(e)  # often already logged in; sessions still work
        sid = client.request("session/new", {"cwd": cwd, "mcpServers": []}, wait=60).get("sessionId")
        result["sessionId"] = sid
        t0 = time.time()
        pr = client.request("session/prompt",
                            {"sessionId": sid, "prompt": [{"type": "text", "text": prompt}]},
                            wait=a.timeout)
        result["stopReason"] = pr.get("stopReason")
        result["elapsed_s"] = round(time.time() - t0, 1)
        result["model_verified"] = verify_model(cwd, sid)
        result["ok"] = True
    except Exception as e:
        result["error"] = str(e); result["trace"] = traceback.format_exc()
    finally:
        open(os.path.join(a.out, "agent_message.txt"), "w").write("".join(client.transcript))
        open(os.path.join(a.out, "tool_calls.txt"), "w").write(
            "\n".join(f"[{k}] {v}" for k, v in client.tool_calls))
        json.dump(result, open(os.path.join(a.out, "result.json"), "w"), indent=2)
        try:
            client.proc.terminate()
        except Exception:
            pass
    print(json.dumps({k: result.get(k) for k in
          ("ok", "stopReason", "model_requested", "model_verified", "elapsed_s", "error")}))


if __name__ == "__main__":
    main()
