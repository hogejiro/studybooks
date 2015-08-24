<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $argv = $_SERVER['argv'];
        if (count($argv) != 2) {
            $this->usage();
            exit(0);
        }

        if ($argv[1] == "plain") {
            $textbuilder = new TextBuilder();
            $director = new Director($textbuilder);
            $director->construct();
            $result = $textbuilder->getResult();
            print(implode("", $result));
        } elseif ($argv[1] == "html") {
            $htmlbuilder = new HTMLBuilder();
            $director = new Director($htmlbuilder);
            $director->construct();
            $result = $htmlbuilder->getResult();
            print(implode("", $result));
        } else {
            $this->usage();
            exit(0);
        }
    }

    public static function usage()
    {
        print("Usage: php main.php plain プレーンテキストで文書作成\n");
        print("Usage: php main.php html  HTMLテキストで文書作成\n");
    }

    public function loadClass($class)
    {
        $file = dirname(__file__) . "/$class.php";
        if (is_readable($file)) {
            require $file;
            return;
        }
    }
}

$ir = new Runner();
$ir->run();
