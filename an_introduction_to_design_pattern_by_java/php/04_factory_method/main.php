<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $factory = new IDCardFactory();
        $card1 = $factory->create("俺");
        $card2 = $factory->create("お前");
        $card3 = $factory->create("大五郎");
        $card1->uses();
        $card2->uses();
        $card3->uses();
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
