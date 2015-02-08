<?php

$config = [
    'pager'       => 'more',
    'usePcntl'    => true,
    'useReadline' => true,
    'commands'    => [
        new \Psy\Command\ParseCommand,
    ],
];

$f = getenv('HOME') . '/.composer/vendor/autoload.php';
if (file_exists($f)) { $config['defaultIncludes'] = [$f]; }

return $config;
