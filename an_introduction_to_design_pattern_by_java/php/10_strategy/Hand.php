<?php

class Hand
{
    const HANDVALUE_GUU = 0;
    const HANDVALUE_CHO = 1;
    const HANDVALUE_PAA = 2;

    const LOST = -1;
    const DRAW = 0;
    const WON  = 1;

    public static $hand = null;
    private static $name = ["グー", "チョキ", "パー"];
    private $handvalue;

    private function __construct($handvalue)
    {
        $this->handvalue = $handvalue;
        if (is_null($this->hand)) {
            $this->hand[] = new Hand(self::HANDVALUE_GUU);
            $this->hand[] = new Hand(self::HANDVALUE_CHO);
            $this->hand[] = new Hand(self::HANDVALUE_PAA);
        }
    }

    public static function getHand($handvalue)
    {
        return $this->hand[$handvalue];
    }

    public function isStrongerThan($h)
    {
        return fight($h) == self::WON;
    }

    public function isWeakerThan($h)
    {
        return fight($h) == self::LOST;
    }

    private function fight($h)
    {
        if ($this == $h) {
            return self::DRAW;
        } elseif (($this->handvalue + 1) % 3 == $h->handvalue) {
            return self::WON;
        } else {
            return self::LOST;
        }
    }

    public function __tostring()
    {
        return $this->name[$this->handvalue];
    }
}
