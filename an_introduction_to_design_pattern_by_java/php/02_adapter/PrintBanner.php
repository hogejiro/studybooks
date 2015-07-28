<?php

class PrintBanner extends Banner implements Prints
{
    public function printWeak()
    {
        $this->showWithParen();
    }

    public function printStrong()
    {
        $this->showWithAster();
    }
}
