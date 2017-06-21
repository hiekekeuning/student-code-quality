package spa;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.text.SimpleDateFormat;
import java.util.Calendar;

/*
 * Extracting code from the data files
 * 
 *  
 *  */
public class ExtractCode 
{
 
	
	public static void extractSourceCodeFile(int year, int month, int day, String inputFolder, String outputFolder) throws IOException
    {
		long t1 = System.currentTimeMillis();
	
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        Calendar cal = Calendar.getInstance();
        cal.set(year, month-1, day); // Months 0 - 11, so 8 is September
        
        String date = dateFormat.format(cal.getTime());
        File index = new File(inputFolder + "/index-" + date);
        File payload = new File(inputFolder + "/payload-" + date);

        //     read the file into a byte array
        FileInputStream fisIndex = new FileInputStream(index);
        byte [] arrIndex = new byte[(int)index.length()];
        fisIndex.read(arrIndex);

        FileInputStream fisPayload = new FileInputStream(payload);
        byte [] arrPayload = new byte[(int)payload.length()];
        fisPayload.read(arrPayload);

        ByteBuffer bufferIndex = ByteBuffer.wrap(arrIndex);
        bufferIndex.order(ByteOrder.BIG_ENDIAN);

        int counter = 0;
        for (int i = 0; i < arrIndex.length; i+=32) {
            long sourceId = bufferIndex.getLong(i);
            long eventId = bufferIndex.getLong(i + 8);
            long start = bufferIndex.getLong(i + 16);
            int length = bufferIndex.getInt(i + 24);
            int success = bufferIndex.getInt(i + 28);
            
            if (success == 1) //only get files that can be compiled
            {
                FileOutputStream fisCode = new FileOutputStream(outputFolder + "/f" + sourceId + "-e" + eventId + ".java");
                fisCode.write(arrPayload, (int)start, length);
                fisCode.close();
                counter++;
            }
            if (counter % 1000 == 0) System.out.print("*");
        }
        System.out.println("Nr of files extracted: " + counter);

        fisIndex.close();
        fisPayload.close();
        
        long t2 = System.currentTimeMillis();
        System.out.println("Execution time\t " + ( (t2-t1)/1000) + " sec" );

    }	

}

