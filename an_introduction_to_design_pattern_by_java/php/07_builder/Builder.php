<?php

abstract class Builder
{
    abstract public function makeTitle($title);
    abstract public function makeString($str);
    abstract public function makeItems($items);
    abstract public function close();
}
