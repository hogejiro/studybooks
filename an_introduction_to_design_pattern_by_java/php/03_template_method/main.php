<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $d1 = new CharDisplay('H');
        $d2 = new StringDisplay("Hello, world.");
        $d3 = new StringDisplay("こんにちは。");
        $d1->display();
        $d2->display();
        $d3->display();
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
