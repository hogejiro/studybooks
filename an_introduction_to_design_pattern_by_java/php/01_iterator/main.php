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
        $it = $bookshelf->iterator();
        while ($it->hasNext()) {
            $book = $it->next();
            printf("%s\n", $book->getName());
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
