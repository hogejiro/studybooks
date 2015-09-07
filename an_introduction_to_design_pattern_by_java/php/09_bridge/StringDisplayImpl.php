<?php

class StringDisplayImpl extends DisplayImpl
{
    private $string;
    private $width;

    public function __construct($string)
    {
        $this->string = $string;
        $this->width = strlen($string);
    }

    public function rawOpen()
    {
        $this->printLine();
    }

    public function rawPrint()
    {
        print("|{$this->string}|\n");
    }

    public function rawClose()
    {
        $this->printLine();
    }

    private function printLine()
    {
        print("+");
        for ($i = 0; $i < $this->width; $i++) {
            print("-");
        }
        print("+\n");
    }
}
