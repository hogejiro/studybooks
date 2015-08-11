<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        print("Start.\n");
        $obj1 = Singleton::getInstance();
        $obj2 = Singleton::getInstance();
        if ($obj1 === $obj2) {
            print("obj1 と obj2 は同じインスタンスです。\n");
        } else {
            print("obj1 と obj2 は同じインスタンスではありません。\n");
        }
        print("End.\n");
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
