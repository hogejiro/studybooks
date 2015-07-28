<?php

class Banner
{
    private $string;

    public function __construct($string)
    {
        $this->string = $string;
    }

    public function showWithParen()
    {
        printf("(%s)\n", $this->string);
    }

    public function showWithAster()
    {
        printf("*%s*\n", $this->string);
    }
}
