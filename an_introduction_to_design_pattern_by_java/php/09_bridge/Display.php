<?php

class Display
{
    private $impl;

    public function __construct(DisplayImpl $impl)
    {
        $this->impl = $impl;
    }

    public function open()
    {
        $this->impl->rawOpen();
    }

    public function print_()
    {
        $this->impl->rawPrint();
    }

    public function close()
    {
        $this->impl->rawClose();
    }

    public final function display()
    {
        $this->open();
        $this->print_();
        $this->close();
    }
}
