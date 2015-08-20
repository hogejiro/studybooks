<?php

class Runner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        // 準備
        $manager = new Manager();
        $upen = new UnderlinePen('~');
        $mbox = new MessageBox('*');
        $sbox = new MessageBox('/');
        $manager->register("strong message", $upen);
        $manager->register("warning box",    $mbox);
        $manager->register("slash box",      $sbox);

        $p1 = $manager->create("strong message");
        $p1->use_("Hello, world.");
        $p2 = $manager->create("warning box");
        $p2->use_("Hello, world.");
        $p3 = $manager->create("slash box");
        $p3->use_("Hello, world.");
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
