<?php

class IDCard extends Product
{
    private $owner;

    public function __construct($owner)
    {
        print $owner . "のカードを作ります。\n";
        $this->owner = $owner;
    }

    public function uses()
    {
        print("{$this->owner}のカードを使います。\n");
    }

    public function getOwner()
    {
        return $this->owner;
    }
}
