<?php
require_once 'Iterator.php';

class BookShelfIterator implements \DP\Iterator
{
    private $bookshelf;
    private $index;

    public function __construct(BookShelf $bookshelf)
    {
        $this->bookshelf = $bookshelf;
        $this->index = 0;
    }

    public function hasNext()
    {
        if ($this->index < $this->bookshelf->getLength()) {
            return true;
        } else {
            return false;
        }
    }

    public function next()
    {
        $book = $this->bookshelf->getBookAt($this->index);
        $this->index++;
        return $book;
    }
}
