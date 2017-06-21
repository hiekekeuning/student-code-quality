package spa;

import java.sql.*;
import educators.database.EducatorsConnection;

public class BlackboxDB extends EducatorsConnection
{
    private static final int minIdYear = 325556185, maxIdYear = 782389849; // 14-15

    private final SpaDB spaDB;

    public static final String dateLimitWeek1 = "created_at BETWEEN '2014-9-8 00:00:00' AND '2014-9-14 23:59:59' ";
    public static final String dateLimitWeek2 = "created_at BETWEEN '2014-12-8 00:00:00' AND '2014-12-14 23:59:59' ";
    public static final String dateLimitWeek3 = "created_at BETWEEN '2015-3-9 00:00:00' AND '2015-3-15 23:59:59' ";
    public static final String dateLimitWeek4 = "created_at BETWEEN '2015-6-8 00:00:00' AND '2015-6-14 23:59:59' ";

    // min and max event ids for sep, dec, mar, jun
    public static final  int [][] weekIds = { {332591825,342090651}, {497097058,509144743}, {609983633,620450771}, {729901911,735813476}};
    
    public BlackboxDB(String dbUrl) throws ClassNotFoundException, SQLException
    {
        super();
        dbConn.setAutoCommit(false);
        spaDB = new SpaDB(dbUrl);
    }

    /*
     * Compile Events
     */

    // stores all successful compile events between 2 ids
    public void getSuccessfulCompileEvents(int fromId, int toId) throws SQLException, ClassNotFoundException
    {
        long t1 = System.currentTimeMillis();

        final String sql = "SELECT m.* FROM master_events m " + "INNER JOIN compile_events c ON c.id = m.event_id "
                + "WHERE m.id >= ? AND m.id <= ? " + "AND m.name = 'compile' " + "AND c.success = true";

        PreparedStatement stat = dbConn.prepareStatement(sql, java.sql.ResultSet.TYPE_FORWARD_ONLY,
                java.sql.ResultSet.CONCUR_READ_ONLY);
        stat.setFetchSize(Integer.MIN_VALUE);
        stat.setInt(1, fromId);
        stat.setInt(2, toId);

        ResultSet res = stat.executeQuery();

        PreparedStatement ps = spaDB.startAddEvents();

        int cnt = 0;

        while (res.next())
        {
            cnt++;

            int sid = res.getInt("m.session_id");
            int seq = res.getInt("m.sequence_num");
            long masterEventId = res.getLong("m.id");
            long userid = res.getLong("user_id");
            Timestamp createdAt = res.getTimestamp("created_at");

            spaDB.addEvent(ps, masterEventId, seq, sid, createdAt, userid);

            if (cnt % 9999 == 0)
            {
                spaDB.commitStat(ps);
                System.out.print("*");
            }
        }
        spaDB.commitStat(ps);

        System.out.println("\nNr of records: " + cnt);
        printElapsedTime(t1);
    }


    /*
     * Snapshots
     */
    public void getSnapshots(int minId, int maxId) throws SQLException, ClassNotFoundException
    {
        long t1 = System.currentTimeMillis();

        String sql = "SELECT ci.source_file_id, m.id FROM master_events m "
                + "INNER JOIN compile_events ce on ce.id = m.event_id "
                + "INNER JOIN compile_inputs ci on ce.id = ci.compile_event_id " + "WHERE m.id >= ? AND m.id <= ? "
                + "AND m.name = 'compile' " + "AND ce.success = true";

        PreparedStatement stat = dbConn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
        stat.setFetchSize(Integer.MIN_VALUE);
        stat.setInt(1, minId);
        stat.setInt(2, maxId);
        ResultSet res = stat.executeQuery();
        int cnt = 0;

        PreparedStatement stmt = spaDB.startAddSnapshot();

        while (res.next())
        {
            long sfid = res.getLong(1);
            long evid = res.getLong(2);

            cnt++;

            spaDB.addFile(stmt, sfid, evid);

            if (cnt % 9999 == 0)
            {
                spaDB.commitStat(stmt);
                System.out.print("*");
            }
        }
        spaDB.commitStat(stmt);
        System.out.println("\nNr of records: " + cnt);
        printElapsedTime(t1);
    }

    /*
     * Extensions
     */

    // all extensions + use in the year 14-15
    public void showAllExtensions1415() throws SQLException
    {
        final String sql = "SELECT name, COUNT(*) " + "FROM extensions "
                + "WHERE master_event_id >= ? AND master_event_id <= ? " + "GROUP BY name " + "ORDER BY COUNT(*) DESC";
        PreparedStatement stat = dbConn.prepareStatement(sql);
        stat.setInt(1, minIdYear);
        stat.setInt(2, maxIdYear);
        ResultSet res = stat.executeQuery();
        printAllCols(res, 2);
    }
    
    // all extensions + use in the 4 selected weeks 14-15
    public void showAllExtensionsSelected4Weeks() throws SQLException
    {
        final String sql = "SELECT name, COUNT(*) " + "FROM extensions "
                + "WHERE (master_event_id >= ? AND master_event_id <= ?) " 
                + "OR (master_event_id >= ? AND master_event_id <= ?) " 
                + "OR (master_event_id >= ? AND master_event_id <= ?) " 
                + "OR (master_event_id >= ? AND master_event_id <= ?) " 
                + "GROUP BY name " + "ORDER BY COUNT(*) DESC";
        PreparedStatement stat = dbConn.prepareStatement(sql);
        int nr = 1;
        for (int[] ids : weekIds)
        {
            stat.setInt(nr, ids[0]);
            stat.setInt(nr+1, ids[1]);
            nr += 2;
        }
        ResultSet res = stat.executeQuery();
        printAllCols(res, 2);
    }

    public void getExtensions(int minId, int maxId) throws SQLException
    {
        long t1 = System.currentTimeMillis();
        String sql = "SELECT master_event_id, name FROM extensions "
                + "WHERE master_event_id >= ? AND master_event_id <= ? "
                + "AND name IN ('Checkstyle', 'PMD', 'FindBugs', 'PMD plug-in', 'PatternCoder')";

        PreparedStatement stat = dbConn.prepareStatement(sql);
        stat.setInt(1, minId);
        stat.setInt(2, maxId);
        ResultSet res = stat.executeQuery();

        PreparedStatement ps = spaDB.startAddExtensions();
        int cnt = 0;
        while (res.next())
        {
            cnt++;
            long evId = res.getLong(1);
            String extName = res.getString(2);

            spaDB.addExtension(ps, evId, extName);
            if (cnt % 9999 == 0 || res.isLast())
            {
                spaDB.commitStat(ps);
                System.out.print("*");
            }
        }
        System.out.println("\nNr of records: " + cnt);
        printElapsedTime(t1);
    }

    public void getStartupEvents(int minId, int maxId) throws SQLException
    {
        long t1 = System.currentTimeMillis();
        String sql = "SELECT session_id, id, user_id, created_at FROM master_events " + "WHERE id >= ? AND id <= ? "
                + "AND name = 'bluej_start'";

        PreparedStatement stat = dbConn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
        stat.setFetchSize(Integer.MIN_VALUE);
        stat.setInt(1, minId);
        stat.setInt(2, maxId);
        ResultSet res = stat.executeQuery();
        int cnt = 0;

        PreparedStatement ps = spaDB.startAddStartupEvents();

        while (res.next())
        {
            int sid = res.getInt(1);
            long masterEventId = res.getLong(2);
            long userid = res.getLong(3);
            Timestamp createdAt = res.getTimestamp(4);

            cnt++;
            spaDB.addStartupEvent(ps, masterEventId, sid, createdAt, userid);

            if (cnt % 9999 == 0)
            {
                spaDB.commitStat(ps);
                System.out.print("*");
            }
        }
        spaDB.commitStat(ps);

        System.out.println("\nNr of records: " + cnt);
        printElapsedTime(t1);
    }

    /*
     * Utils
     * 
     */
    private void printElapsedTime(long t1)
    {
        long t2 = System.currentTimeMillis();
        System.out.println("Execution time\t " + ((t2 - t1) / 1000) + " sec");
    }

    private void printAllCols(ResultSet res, int nrCols) throws SQLException
    {
        while (res.next())
        {
            for (int i = 1; i <= nrCols; i++)
            {
                System.out.print(res.getString(i) + ";");
            }
            System.out.println();
        }
    }

    /*
     * Info
     */
    public void showEventInfo(final String dateRange) throws SQLException
    {
        final String sql = "SELECT count(*) FROM master_events m " + "INNER JOIN compile_events c ON c.id = m.event_id "
                + "WHERE " + dateRange + "AND m.name = 'compile' " + "AND c.success = true";

        Statement stat = dbConn.createStatement();
        ResultSet res = stat.executeQuery(sql);
        printAllCols(res, 1);
    }

    public void showNrOfStartupEvents1415() throws SQLException
    {
        final String sql = "SELECT count(*) FROM master_events " + "WHERE id >= ? AND id <= ? "
                + "AND name = 'bluej_start'";
        PreparedStatement stat = dbConn.prepareStatement(sql);
        stat.setInt(1, minIdYear);
        stat.setInt(2, maxIdYear);
        ResultSet res = stat.executeQuery();
        printAllCols(res, 1);
    }
    
    public void showNrOfStartupEventsFourWeeks() throws SQLException
    {
        final String sql = "SELECT count(*) FROM master_events " 
                + "WHERE ((id >= ? AND id <= ?) "
                + "OR (id >= ? AND id <= ?) " 
                + "OR (id >= ? AND id <= ?) " 
                + "OR (id >= ? AND id <= ?)) " 
                + "AND name = 'bluej_start'";
        PreparedStatement stat = dbConn.prepareStatement(sql);
        int nr = 1;
        for (int[] ids : weekIds)
        {
            stat.setInt(nr, ids[0]);
            stat.setInt(nr+1, ids[1]);
            nr += 2;
        }
        ResultSet res = stat.executeQuery();
        printAllCols(res, 1);
    }

    /*
     * Get the min and max ids for events
     */
    public void getIdInfo(final String dateRange) throws SQLException
    {
        // per day, all events
        String sql = "SELECT min(id), max(id), date(created_at) FROM master_events " + "WHERE " + dateRange
                + "GROUP BY date(created_at)";
        Statement stat = dbConn.createStatement();
        ResultSet results = stat.executeQuery(sql);
        while (results.next())
        {
            Date d = results.getDate(3);
            int min = results.getInt(1);
            int max = results.getInt(2);
            System.out.println(d + ";" + min + ";" + max);
        }

        // per week, only compile events
        String sql2 = "SELECT min(id), max(id), count(*) FROM master_events " + "WHERE " + dateRange
                + "AND event_type='CompileEvent'";
        stat = dbConn.createStatement();
        results = stat.executeQuery(sql2);
        while (results.next())
        {
            int cnt = results.getInt(3);
            int min = results.getInt(1);
            int max = results.getInt(2);
            System.out.println(dateRange + ";" + min + ";" + max + "=" + cnt);
        }

    }

  

}
