<?php

class UnderlinePen extends Product
{
    private $ulchar;

    public function __construct($ulchar)
    {
        $this->ulchar = $ulchar;
    }

    public function use_($s)
    {
        $length = strlen($s);
        print("\"" . $s . "\"\n");
        print(" ");
        for ($i = 0; $i < $length; $i++)
        {
            print($this->ulchar);
        }
        print("\n");
    }

    public function createClone()
    {
        $p = null;
        try {
            $p = clone $this;
        } catch (Exception $e) {
            print($e->getMessage() . "\n");
        }
        return $p;
    }
}
