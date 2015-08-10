<?php

class IDCardFactory extends Factory
{
    private $owners = [];

    protected function createProduct($owner)
    {
        return new IDCard($owner);
    }

    protected function registerProduct($product)
    {
        $this->owners[] = $product->getOwner();
    }

    public function getOwners()
    {
        $this->owners;
    }
}
