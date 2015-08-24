<?php

class TextBuilder extends Builder
{
    private $buffer;

    public function __construct()
    {
        $this->buffer = [];
    }

    public function makeTitle($title)
    {
        $this->buffer[] = "===============================\n";
        $this->buffer[] = "[$title]\n";
        $this->buffer[] = "\n";
    }

    public function makeString($str)
    {
        $this->buffer[] = "*$str\n";
        $this->buffer[] = "\n";
    }

    public function makeItems($items)
    {
        foreach($items as $item) {
            $this->buffer[] = "  ・$item\n";
        }
        $this->buffer[] = "\n";
    }

    public function close()
    {
        $this->buffer[] = "===============================\n";
    }

    public function getResult()
    {
        return $this->buffer;
    }
}
