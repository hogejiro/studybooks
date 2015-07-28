<?php

class AdapterRunner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $p = new PrintBanner("Hello");
        $p->printWeak();
        $p->printStrong();

        // using delegation
        $p_ = new PrintBanner_("Hello");
        $p_->printWeak();
        $p_->printStrong();
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

$ir = new AdapterRunner();
$ir->run();
