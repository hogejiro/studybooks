<?php
require_once 'Aggregate.php';

class BookShelf implements Aggregate
{
    private $books = [];
    private $last  = 0;

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

    public function iterator()
    {
        return new BookShelfIterator($this);
    }

    public function reverseIterator()
    {
        return new BookShelfReverseIterator($this);
    }
}
