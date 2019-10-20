import java.io.*;
import java.util.Vector;

/**
 * 文件工具类
 */
public class FileUtil {
	public static void main(String[] args) throws FileNotFoundException {
		String s = readFile( new FileInputStream("/home/hadoop001/Desktop/test.data") );
		System.out.println(s);
	}
	/**
	 * 读取文件内容
	 *
	 * @param is
	 * @return
	 */
	public static String readFile(InputStream is) {
		BufferedReader br = null;
		StringBuffer sb = new StringBuffer();
		try {
			br = new BufferedReader(new InputStreamReader(is, "UTF-8"));
			String readLine = null;
			while ((readLine = br.readLine()) != null) {
				sb.append(readLine+"\r\n");
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				br.close();
				is.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return sb.toString();
	}
}