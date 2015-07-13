<?php

class IteratorRunner
{
    public function __construct()
    {
        spl_autoload_register(array($this, 'loadClass'));
    }

    public function run()
    {
        $bookshelf = new BookShelf(4);
        $bookshelf->appendBook(new Book("Around the world in 80 Days"));
        $bookshelf->appendBook(new Book("Bible"));
        $bookshelf->appendBook(new Book("Cinderella"));
        $bookshelf->appendBook(new Book("Daddy-Long-Legs"));

        $iterator = $bookshelf->iterator();
        foreach ($iterator as $it) {
            printf("%s\n", $it->getName());
        }

        // reverse
        $r_iterator = $bookshelf->reverseIterator();
        foreach ($r_iterator as $it) {
            printf("%s\n", $it->getName());
        }
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

$ir = new IteratorRunner();
$ir->run();
