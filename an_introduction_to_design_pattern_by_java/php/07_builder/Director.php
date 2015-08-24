<?php

class Director
{
    private $builder;

    public function __construct($builder)
    {
        $this->builder = $builder;
    }

    public function construct()
    {
        $this->builder->makeTitle("Greeting");
        $this->builder->makeString("朝から昼にかけて");
        $this->builder->makeItems([
            "おはようございます。",
            "こんにちは。",
        ]);
        $this->builder->makeString("夜に");
        $this->builder->makeItems([
            "こんばんは。",
            "おやすみなさい。",
            "さようなら。",
        ]);
        $this->builder->close();
    }
}
