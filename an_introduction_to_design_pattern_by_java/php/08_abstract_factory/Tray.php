<?php

abstract class Tray extends Item
{
    protected $tray = [];

    public function __construct($caption)
    {
        parent::__construct($caption);
    }

    public function add($item)
    {
        $this->tray[] = $item;
    }
}
