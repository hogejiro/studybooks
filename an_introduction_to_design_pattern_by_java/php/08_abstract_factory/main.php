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
            print("Usage: php main.php classname_of_concreate_factory\n");
            print("Example 1: php main.php ListFactory\n");
            print("Example 2: php main.php TableFactory\n");
            exit(0);
        }
        $factory = Factory::getFactory($argv[1]);

        $asahi    = $factory->createLink("朝日新聞",    "http://www.asahi.com/");
        $yomiuri  = $factory->createLink("読売新聞",    "http://www.yomiuri.co.jp/");
        $us_yahoo = $factory->createLink("Yahoo!",      "http://www.yahoo.com/");
        $jp_yahoo = $factory->createLink("Yahoo!Japan", "http://www.yahoo.co.jp/");
        $excite   = $factory->createLink("Excite",      "http://www.excite.com/");
        $google   = $factory->createLink("Google",      "http://www.google.com/");

        $traynews = $factory->createTray("新聞");
        $traynews->add($asahi);
        $traynews->add($yomiuri);

        $trayyahoo = $factory->createTray("Yahoo!");
        $trayyahoo->add($us_yahoo);
        $trayyahoo->add($jp_yahoo);

        $traysearch = $factory->createTray("サーチエンジン");
        $traysearch->add($trayyahoo);
        $traysearch->add($excite);
        $traysearch->add($google);

        $page = $factory->createPage("LinkPage", "結城 浩");
        $page->add($traynews);
        $page->add($traysearch);
        $page->output();
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
