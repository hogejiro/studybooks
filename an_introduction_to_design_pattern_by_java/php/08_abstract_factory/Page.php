<?php
abstract class Page
{
    protected $title;
    protected $author;
    protected $content = [];

    public function __construct($title, $author)
    {
        $this->title = $title;
        $this->author = $author;
    }

    public function add($item)
    {
        $this->content[] = $item;
    }

    public function output()
    {
        try {
            $filename = $this->title . ".html";
            $fp = fopen($filename, "w");
            fwrite($fp, $this->makeHTML());
            fclose($fp);
            print("{$filename}を作成しました。\n");
        } catch (IOException $e) {
            print $e->getMessage() . "\n";
        }
    }

    abstract function makeHTML();
}
