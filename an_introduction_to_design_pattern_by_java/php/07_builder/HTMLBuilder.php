<?php

class HTMLBuilder extends Builder
{
    private $filename;
    private $writer;

    public function __construct()
    {
        $this->buffer = [];
    }

    public function makeTitle($title)
    {
        $this->buffer[] = "<html><head><title>$title</title></head><body>\n";
        $this->buffer[] = "<h1>$title</h1>\n";
    }

    public function makeString($str)
    {
        $this->buffer[] = "<p>$str</p>\n";
    }

    public function makeItems($items)
    {
        $this->buffer[] = "<ul>\n";
        foreach($items as $item) {
            $this->buffer[] = "<li>$item</li>\n";
        }
        $this->buffer[] = "</ul>\n";
    }

    public function close()
    {
        $this->buffer[] = "</body></html>\n";
    }

    public function getResult()
    {
        return $this->buffer;
    }
}
