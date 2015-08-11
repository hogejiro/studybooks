<?php

class Singleton
{
    private function __construct()
    {
        print("インスタンスを生成しました。\n");
    }

    public static function getInstance()
    {
        static $singleton;
        if (is_null($singleton)) {
            $singleton = new Singleton();
        }
        return $singleton;
    }
}
