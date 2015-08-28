<?php

class TableLink extends Link
{
    public function __construct($caption, $url)
    {
        parent::__construct($caption, $url);
    }

    public function makeHTML()
    {
        return "  <td><a href=\"{$this->url}\">{$this->caption}</a></td>\n";
    }
}
