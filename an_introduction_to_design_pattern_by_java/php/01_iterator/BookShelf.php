<?php

class BookShelf implements IteratorAggregate
{
    private $books = [];

    public function getBookAt($index)
    {
        return $this->books[$index];
    }

    public function appendBook(Book $book)
    {
        $this->books[] = $book;
    }

    public function getLength()
    {
        return count($this->books);
    }

    public function getIterator()
    {
        return new BookShelfIterator($this);
    }

    public function getReverseIterator()
    {
        return new BookShelfReverseIterator($this);
    }
}
