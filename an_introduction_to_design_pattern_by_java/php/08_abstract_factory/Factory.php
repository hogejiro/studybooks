<?php

abstract class Factory
{
    public static function getFactory($classname)
    {
        $factory = null;
        try {
            $factory = new $classname();
        } catch (ClassNotFoundException $e) {
            print("クラス $classname が見つかりません。\n");
        } catch (Exception $e) {
            print($e->getMessage() . "\n");
        }
        return $factory;
    }
    abstract public function createLink($caption, $url);
    abstract public function createTray($caption);
    abstract public function createPage($title, $author);
}
