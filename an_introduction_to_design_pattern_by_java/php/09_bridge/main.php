<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $d1 = new      Display(new StringDisplayImpl("Hello, Japan."));
        $d2 = new CountDisplay(new StringDisplayImpl("Hello, Japan."));
        $d3 = new CountDisplay(new StringDisplayImpl("Hello, Universe."));
        $d1->display();
        $d2->display();
        $d3->display();
        $d3->multiDisplay(5);
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
