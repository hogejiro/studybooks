<?php

class BookShelfIterator implements Iterator
{
    protected $bookshelf;
    protected $index;

    public function __construct(BookShelf $bookshelf)
    {
        $this->bookshelf = $bookshelf;
        $this->index = 0;
    }

    public function current()
    {
        return $this->bookshelf->getBookAt($this->index);
    }

    public function key()
    {
        return $this->index;
    }

    public function next()
    {
        $book = $this->bookshelf->getBookAt($this->index);
        $this->index++;
        return $book;
    }

    public function rewind()
    {
        $this->index = 0;
    }

    public function valid()
    {
        if ($this->index < $this->bookshelf->getLength()) {
            return true;
        } else {
            return false;
        }
    }
}
