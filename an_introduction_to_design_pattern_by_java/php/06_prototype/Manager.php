<?php

class Manager
{
    private $showcase = array();

    public function register($name, $proto)
    {
        $this->showcase[$name] = $proto;
    }

    public function create($protoname)
    {
        $p = $this->showcase[$protoname];
        return $p->createClone();
    }
}
