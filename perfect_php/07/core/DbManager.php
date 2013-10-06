<?php

class DbManager
{
    protected $connections = array();
    protected $repository_connection_map = array();
    protected $repositories = array();

    public function connect($name, $params)
    {
        $default_params = array(
            'dsn'      => null,
            'user'     => '',
            'password' => '',
            'options'  => array(),
        );
        $params = array_merge($default_params, $params);
        $conn = new PDO(
            $params['dsn'],
            $params['user'],
            $params['password'],
            $params['options']
        );
        $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        $this->connections[$name] = $conn;
    }

    public function getConnection($name = null)
    {
        if (is_null($name)) {
            return current($this->connections);
        }
        return $this->connections[$name];
    }

    public function setRepositoryConnectionMap($repository_name, $name)
    {
        $this->repository_connection_map[$repository_name] = $name;
    }

    public function getConnectionForRepository($repository_name)
    {
        if (isset($this->repository_connection_map[$repository_name])) {
            $name = $this->repository_connection_map[$repository_name];
            $conn = $this->getConnection($name);
        } else {
            $conn = $this->getConnection();
        }
        return $conn;
    }

    public function getRepository($repository_name)
    {
        if (! isset($this->repositories[$repository_name])) {
            $repository_class = $repository_name . 'Repository';
            $conn = $this->getConnectionForRepository($repository_name);
            $repository = new $repository_class($conn);
            $this->repositories[$repository_name] = $repository;
        }
        return $this->repositories[$repository_name];
    }

    public function __destruct()
    {
        foreach ($this->repositories as $repository) {
            unset($repository);
        }
        foreach ($this->connections as $conn) {
            unset($conn);
        }
    }
}
