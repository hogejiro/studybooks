<?php

class BookShelfReverseIterator extends BookShelfIterator
{
    public function next()
    {
        $book = $this->bookshelf->getBookAt($this->index);
        $this->index--;
        return $book;
    }

    public function rewind()
    {
        $this->index = $this->bookshelf->getLength() - 1;
    }

    public function valid()
    {
        if ($this->index >= 0) {
            return true;
        } else {
            return false;
        }
    }
}
