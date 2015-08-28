<?php

class TableTray extends Tray
{
    public function __construct($caption)
    {
        parent::__construct($caption);
    }

    public function makeHTML()
    {
        $buffer = [];
        $buffer[] = "<td>\n";
        $buffer[] = "<table width=\"100%\" border=\"1\"><tr>";
        $tray_size = count($this->tray);
        $buffer[] = "<td bgcolor=\"#cccccc\" align=\"center\" colspan=\"$tray_size\"><b>{$this->caption}</b></td>\n";
        $buffer[] = "</tr>\n";
        $buffer[] = "<tr>\n";
        foreach($this->tray as $t) {
            $buffer[] = $t->makeHTML();
        }
        $buffer[] = "</tr></table>\n";
        $buffer[] = "</td>\n";
        return implode("", $buffer);
    }
}
