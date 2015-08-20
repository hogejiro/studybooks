<?php

abstract class Product
{
    private $_values = array();
    public function __set($key, $value)
    {
        $this->_values[$key] = $value;
    }

    public function __get($key)
    {
        return $this->_values[$key];
    }

    public function __clone()
    {
        foreach ($this->_values as $key => $value) {
            if (is_object($value)) {
                $this->$key = clone $value;
            } else {
                $this->$key = $value;
            }
        }
    }

    abstract public function use_($s);
    abstract public function createClone();
}
