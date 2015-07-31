<?php

class StringDisplay extends AbstractDisplay
{
    private $string;
    private $width;

    public function __construct($string)
    {
        $this->string = $string;
        $this->width  = strlen($string);
    }

    public function open()
    {
        $this->printLine();
    }

    public function prints()
    {
        print("|{$this->string}|\n");
    }

    public function close()
    {
        $this->printLine();
    }

    public function printLine()
    {
        print("+");
        for ($i = 0; $i < $this->width; $i++) {
            print("-");
        }
        print("+\n");
    }
}
