<?php

Abstract class AbstractDisplay
{
    abstract public function open();
    abstract public function prints();
    abstract public function close();
    final public function display()
    {
        $this->open();
        for ($i = 0; $i < 5; $i++) {
            $this->prints();
        }
        $this->close();
    }
}
