<?php

class ListPage extends Page
{
    public function __construct($title, $author)
    {
        parent::__construct($title, $author);
    }

    public function makeHTML()
    {
        $buffer = [];
        $buffer[] = "<html><head><title>{$this->title}</title></head>\n";
        $buffer[] = "<body>\n";
        $buffer[] = "<h1>{$this->title}</h1>\n";
        $buffer[] = "<ul>\n";
        foreach($this->content as $t) {
            $buffer[] = $t->makeHTML();
        }
        $buffer[] = "</ul>\n";
        $buffer[] = "<hr><address>{$this->author}</address>\n";
        $buffer[] = "</body></html>\n";
        return implode("", $buffer);
    }
}
