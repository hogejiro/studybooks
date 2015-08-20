<?php

class MessageBox extends Product
{
    private $decochar;

    public function __construct($decochar)
    {
        $this->decochar = $decochar;
    }

    public function use_($s)
    {
        $length = strlen($s);
        for ($i = 0; $i < $length + 4; $i++)
        {
            print($this->decochar);
        }
        print("\n");
        print($this->decochar . " " . $s . " " . $this->decochar . "\n");
        for ($i = 0; $i < $length + 4; $i++)
        {
            print($this->decochar);
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
