<?php
use PHPUnit\Framework\TestCase;
use PHPUnit\DbUnit\TestCaseTrait;
use PHPUnit\DbUnit\DataSet\CsvDataSet;

use PHPUnit\DbUnit\DataSet\AbstractDataSet;
use PHPUnit\DbUnit\DataSet\DefaultTable;
use PHPUnit\DbUnit\DataSet\DefaultTableMetaData;
use PHPUnit\DbUnit\DataSet\DefaultTableIterator;

use PHPUnit\DbUnit\DataSet\QueryDataSet;

use PHPUnit\DbUnit\DataSet\ReplacementDataSet;

use PHPUnit\DbUnit\DataSet\Filter;

use PHPUnit\DbUnit\DataSet\CompositeDataSet;

class DbUnitExtTest extends TestCase{
	use TestCaseTrait;
	static private $pdo = null;
	private $conn = null;
	public function getConnection(){
		if (self::$pdo == null) {
			self::$pdo = new PDO( $GLOBALS['DB_DSN'], $GLOBALS['DB_USER'], $GLOBALS['DB_PASSWD'] );
		}
		$this->conn = $this->createDefaultDBConnection(self::$pdo, $GLOBALS['DB_DBNAME']);
		return $this->conn;
	}
	public function getDataSet(){
		$dataSet = new CsvDataSet();
		$dataSet->addTable('test', dirname(__FILE__)."/users.csv");
		return $dataSet;
	}

	public function testData(){
		$dataSet = new CsvDataSet();
		$dataSet->addTable('test', dirname(__FILE__)."/users.csv");
		$queryTable = $this->getConnection()->createQueryTable(
			    'test', 'select * from test'
			);
		$this->assertTablesEqual($dataSet->getTable("test"), $queryTable);
	}
	public function testArrayAndQuery() {
		$arrayDataSet = new MyApp_DbUnit_ArrayDataSet(array(
			"test" => array(
				array(
					"id" => 1,
					"name" => "liqingshou",
					"astro" => "hello world"
				),
				array(
					"id" => 2,
					"name" => "soso",
					"astro" => "soso2501@gmail.com"
			),
		)));
		$queryDataSet = new QueryDataSet($this->getConnection());
		$queryDataSet->addTable("test", "select * from test");
		$this->assertTablesEqual($arrayDataSet->getTable("test"), $queryDataSet->getTable("test"));
	}
	public function testReplace(){
		$dataSet = new CsvDataSet();
		$dataSet->addTable('test', dirname(__FILE__)."/usersWithReplace.csv");
		$rds = new ReplacementDataSet($dataSet);
		$rds->addFullReplacement('##NAME##', "soso");
		$dbDataSet = $this->getConnection()->createDataSet(array("test"));
		$this->assertTablesEqual($dbDataSet->getTable("test"), $rds->getTable("test"));
	}
	public function testFilter(){
		$dbDataSet = $this->getConnection()->createDataSet(array("test"));
		$filter = new Filter($dbDataSet);
		$filter->setIncludeColumnsForTable("test", array("id"));

		$queryDataSet = new QueryDataSet($this->getConnection());
		$queryDataSet->addTable("test", "select id from test");

		$this->assertTablesEqual($queryDataSet->getTable("test"), $filter->getTable("test"));
	}
	public function testComposite(){
		$composite = new CompositeDataSet();
		$composite->addDataSet(new MyApp_DbUnit_ArrayDataSet(array(
			"test" => array(
				array(
					"id" => 1,
					"name" => "liqingshou",
					"astro" => "hello world"
				)
		))));
		$composite->addDataSet(new MyApp_DbUnit_ArrayDataSet(array(
			"test" => array(
				array(
					"id" => 2,
					"name" => "soso",
					"astro" => "soso2501@gmail.com"
			),
		))));
		$dataSet = new CsvDataSet();
		$dataSet->addTable('test', dirname(__FILE__)."/users.csv");
		$this->assertTablesEqual($composite->getTable("test"), $dataSet->getTable("test"));
	}

	public function testConnection(){
		$con = $this->getConnection();
		$this->assertEquals($con->getRowCount("test", "id=1"), 1);
		$dataSet = new CsvDataSet();
		$dataSet->addTable('test', dirname(__FILE__)."/users.csv");
		$this->assertDataSetsEqual($con->createDataSet(), $dataSet);
		$this->assertTablesEqual($con->createQueryTable("test", "select * from test"), $dataSet->getTable("test"));
	}
}

// 自定义DataSet类
class MyApp_DbUnit_ArrayDataSet extends AbstractDataSet {
	/**
	 * @var array
	 */
	protected $tables = array();

	/**
	 * @param array $data
	 */
	public function __construct(array $data){
		foreach ($data as $tableName => $rows) {
			$columns = array();
			if (isset($rows[0])) {
				$columns = array_keys($rows[0]);
			}

			$metaData = new DefaultTableMetaData($tableName, $columns);
			$table = new DefaultTable($metaData);

			foreach ($rows AS $row) {
				$table->addRow($row);
			}
			$this->tables[$tableName] = $table;
		}
	}

	protected function createIterator($reverse = FALSE)
	{
		return new DefaultTableIterator($this->tables, $reverse);
	}

	/*
	public function getTable($tableName)
	{
		if (!isset($this->tables[$tableName])) {
			throw new InvalidArgumentException("$tableName is not a table in the current database.");
		}
		return $this->tables[$tableName];
	}
	*/
}

