<?php

abstract class DbRepository
{
    protected $conn;

    public function __construct($conn)
    {
        $this->setConnection($conn);
    }

    public function setConnection($conn)
    {
        $this->conn = $conn;
    }

    public function execute($sql, $params = array())
    {
        $stmt = $this->conn->prepare($sql);
        $stmt->execute($params);
        return $stmt;
    }

    public function fetch($sql, $params = array())
    {
        return $this->execute($sql, $params)->fetch(PDO::FETCH_ASSOC);
    }

    public function fetchAll($sql, $params = array())
    {
        return $this->execute($sql, $params)->fetchAll(PDO::FETCH_ASSOC);
    }
}
