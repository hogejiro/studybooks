<?php

class ListLink extends Link
{
    public function __construct($caption, $url)
    {
        parent::__construct($caption, $url);
    }

    public function makeHTML()
    {
        return "  <li><a href=\"{$this->url}\">{$this->caption}</a></li>\n";
    }
}
