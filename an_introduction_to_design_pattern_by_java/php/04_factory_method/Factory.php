<?php

Abstract class Factory
{
    final public function create($owner)
    {
        $p = $this->createProduct($owner);
        $this->registerProduct($p);
        return $p;
    }

    abstract protected function createProduct($owner);
    abstract protected function registerProduct($product);
}
