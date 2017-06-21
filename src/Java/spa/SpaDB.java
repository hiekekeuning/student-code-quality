package spa;

import java.io.*;
import java.sql.*;

public class SpaDB
{
	protected Connection conn;
		
	public SpaDB(String dbUrl) throws ClassNotFoundException, SQLException
	{
		Class.forName("org.sqlite.JDBC");
        this.conn = DriverManager.getConnection(dbUrl);	
        conn.setAutoCommit(false); //for batch inserts
        System.out.println("Connected to Sqlite db");
	}
	
	/*
	 * Creates the input file for Fixing.issueFixing:
	 * sf1, iss1, 3,    ev1
	 * sf1, iss1, 2,    ev2
	 * sf1, iss2, null, ev1 
	 * sf1, iss2, 1,    ev2
	 * sf2, iss1, null, ev3
	 * sf2, iss2, 3,    ev3 
	 * sf3, iss1, 5,    ev4 
	 * .. 
	 */
	public void issueFixing(String outFileName) throws SQLException, FileNotFoundException
	{
		String sql = "SELECT sf_id, n, count, s.event_id "
			+ "FROM (snapshot s INNER JOIN (SELECT DISTINCT name n from issue3) ) "
			+ "LEFT JOIN issue3 i ON (i.event_id=s.event_id AND sf_id=source_file_id AND i.name=n) "
			+ "ORDER BY sf_id, n, s.event_id;";
		
        PreparedStatement stmt = conn.prepareStatement(sql);
        
        PrintWriter pw = new PrintWriter(new File(outFileName));
        StringBuilder sb = new StringBuilder();
        ResultSet res = stmt.executeQuery();
        int cnt=0;
        while (res.next())
        {
        	long sfid = res.getLong(1);
        	String issue= res.getString(2);
        	int count = res.getInt(3);
        	long evid = res.getLong(4);
        	
        	String rec = sfid + "," + issue + "," + count + "," + evid + '\n';
        	sb.append(rec);
        	if (cnt % 99999 == 0) 
          	{
          		System.out.println("* " + cnt);
          		pw.write(sb.toString());
          		sb.setLength(0);
          	}
        	cnt++;
        }
        pw.write(sb.toString());
        System.out.println("\nWrote to csv: " + cnt);//
        pw.close();
	}
        
	public void commitStat(PreparedStatement ps) throws SQLException
	{
		ps.executeBatch();
		conn.commit();
	}
		
	// Events
	public PreparedStatement startAddEvents() throws SQLException
	{
		String sql = "INSERT INTO event VALUES (?, ?, ?, ?, ?)";
        return conn.prepareStatement(sql);
	}
	
	public void addEvent(PreparedStatement stmt, long id, int seqNum, long session_id, Timestamp createdAt, long userId) throws SQLException
	{
        stmt.setLong(1, id);
        stmt.setLong(2, seqNum);
        stmt.setLong(3, session_id);
        stmt.setTimestamp(4, createdAt);
        stmt.setLong(5, userId);
        
        stmt.addBatch();
	}
	
	// Startup events
	public PreparedStatement startAddStartupEvents() throws SQLException
	{
		String sql = "INSERT INTO startup_event VALUES (?, ?, ?, ?)";
        return conn.prepareStatement(sql);
	}
	
	public void addStartupEvent(PreparedStatement stmt, long id, long session_id, Timestamp createdAt, long userId) throws SQLException
	{
        stmt.setLong(1, id);
        stmt.setLong(2, session_id);
        stmt.setTimestamp(3, createdAt);
        stmt.setLong(4, userId);
        
        stmt.addBatch();
	}

	// Snapshots
	public PreparedStatement startAddSnapshot() throws SQLException
	{
		String sql = "INSERT INTO snapshot VALUES (?, ?, ?)";
        return conn.prepareStatement(sql);
	}
	
	public void addFile(PreparedStatement stmt, long sfid, long evid) throws SQLException
	{
        stmt.setLong(1, sfid);
        stmt.setLong(2, evid);
        stmt.setString(3, "f" + sfid + "-e" + evid + ".java"); 
        stmt.addBatch();
	}
	
	// Extensions 
	public PreparedStatement startAddExtensions() throws SQLException
	{
		String sql = "INSERT INTO extension VALUES (?, ?)";
        return conn.prepareStatement(sql);
	}
	
	public void addExtension(PreparedStatement stmt, long evId, String name) throws SQLException
	{
        stmt.setLong(1, evId);
        stmt.setString(2, name);
        stmt.addBatch();
	}

}


