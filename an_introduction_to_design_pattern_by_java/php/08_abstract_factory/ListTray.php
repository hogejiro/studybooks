<?php

class ListTray extends Tray
{
    public function __construct($caption)
    {
        parent::__construct($caption);
    }

    public function makeHTML()
    {
        $buffer = [];
        $buffer[] = "<li>\n";
        $buffer[] = "{$this->caption}\n";
        $buffer[] = "<ul>\n";
        foreach($this->tray as $t) {
            $buffer[] = $t->makeHTML();
        }
        $buffer[] = "</ul>\n";
        $buffer[] = "</li>\n";
        return implode("", $buffer);
    }
}
