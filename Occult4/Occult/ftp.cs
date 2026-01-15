using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Net;
using System.Windows.Forms;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult
{
	public class ftp
	{
		internal static string AppPath;

		internal static bool CancelFlag;

		public static List<string> GetFtpFolderNames(string ftpUri, string username, string password)
		{
			List<string> list = new List<string>();
			try
			{
				FtpWebRequest obj = (FtpWebRequest)WebRequest.Create(ftpUri);
				obj.Method = "LIST";
				obj.Credentials = new NetworkCredential(username, password);
				using FtpWebResponse ftpWebResponse = (FtpWebResponse)obj.GetResponse();
				using Stream stream = ftpWebResponse.GetResponseStream();
				using StreamReader streamReader = new StreamReader(stream);
				string text;
				while ((text = streamReader.ReadLine()) != null)
				{
					if (text.StartsWith("d"))
					{
						string[] array = text.Split(new char[1] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
						if (array.Length > 8)
						{
							string item = array[8];
							list.Add(item);
						}
					}
				}
				return list;
			}
			catch (WebException ex)
			{
				Console.WriteLine("FTP Error: " + ex.Message);
				return list;
			}
			catch (Exception ex2)
			{
				Console.WriteLine("An unexpected error occurred: " + ex2.Message);
				return list;
			}
		}

		internal static bool DownloadFTP(string Server, string FileName, string FinalDestination, bool unzip, bool gunzip, bool SupressMessages)
		{
			//IL_006d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Invalid comparison between Unknown and I4
			//IL_0088: Unknown result type (might be due to invalid IL or missing references)
			//IL_0390: Unknown result type (might be due to invalid IL or missing references)
			//IL_0578: Unknown result type (might be due to invalid IL or missing references)
			//IL_0598: Unknown result type (might be due to invalid IL or missing references)
			string userName;
			string password;
			if (Server.IndexOf("astro.cz") >= 0)
			{
				userName = "mpcorb";
				password = "Ceres";
			}
			else if (FileName.Contains("de_6000yrs.zip"))
			{
				userName = Output.UnHide("k*&,+,-&(3z{", 30);
				password = Output.UnHide("q}}krjz{", 30);
			}
			else
			{
				userName = "anonymous";
				if (Settings.Default.FTP_AnonymousPassword.Length < 4)
				{
					if ((int)MessageBox.Show("You must specify your Email address for anonymous FTP", "No email address", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
					{
						return false;
					}
					Defaults defaults = new Defaults();
					((Control)defaults.FTPPassword).Focus();
					((Form)defaults).ShowDialog();
				}
				if (Settings.Default.FTP_AnonymousPassword.Length < 4)
				{
					return false;
				}
				password = Settings.Default.FTP_AnonymousPassword;
			}
			FtpWebResponse ftpWebResponse = null;
			Stream stream = null;
			Stream stream2 = null;
			PBar pBar = null;
			bool flag = false;
			try
			{
				FtpWebRequest obj = (FtpWebRequest)WebRequest.Create(new Uri(new Uri(Server), FileName));
				obj.Credentials = new NetworkCredential(userName, password);
				obj.Method = "RETR";
				obj.UseBinary = true;
				ftpWebResponse = (FtpWebResponse)obj.GetResponse();
				int num = (int)ftpWebResponse.ContentLength;
				if (num < 0)
				{
					if (FileName.Contains("de_6000yrs.zip"))
					{
						num = 648400000;
					}
					if (Server.IndexOf("mpcorb.klet") >= 0)
					{
						num = 40000000;
					}
				}
				stream = ftpWebResponse.GetResponseStream();
				string path = ((!(unzip || gunzip)) ? (FinalDestination + FileName) : (AppPath + "\\Downloaded files\\" + FileName));
				if (((FileName.ToLower() == "future.dat") | (FileName.ToLower() == "futureall540.zip")) && Settings.Default.PreserveFuture_dat)
				{
					string sourceFileName = FinalDestination + Path.GetFileName(path)!.Replace("zip", "dat");
					DateTime dateTime = DateTime.Now.ToUniversalTime();
					string text = FinalDestination + Path.GetFileNameWithoutExtension(path) + "_" + dateTime.Year + dateTime.Month.ToString().PadLeft(2, '0') + dateTime.Day.ToString().PadLeft(2, '0') + Path.GetExtension(path)!.Replace("zip", "dat");
					if (!File.Exists(text))
					{
						File.Move(sourceFileName, text);
					}
				}
				if (File.Exists(path))
				{
					File.Delete(path);
				}
				stream2 = File.OpenWrite(path);
				pBar = new PBar();
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Value(0);
				pBar.pBarFTP.set_Maximum(num + 50);
				((Control)pBar).set_Text("Downloading " + FileName);
				((Form)pBar).set_StartPosition((FormStartPosition)1);
				((Form)pBar).set_TopMost(true);
				((Control)pBar).Show();
				CancelFlag = false;
				byte[] buffer = new byte[4096];
				int num2 = 0;
				int num3 = 0;
				while ((num2 = stream.Read(buffer, 0, 4096)) > 0)
				{
					stream2.Write(buffer, 0, num2);
					num3 = (int)stream2.Length;
					if (num3 > pBar.pBarFTP.get_Maximum())
					{
						pBar.pBarFTP.set_Maximum(2 * num3);
					}
					pBar.pBarFTP.set_Value(num3);
					pBar.Devents();
					Application.DoEvents();
					if (CancelFlag)
					{
						break;
					}
				}
				((Form)pBar).Close();
				flag = true;
				if (CancelFlag)
				{
					flag = false;
					stream.Close();
					stream2.Close();
					ftpWebResponse.Close();
				}
			}
			catch (Exception ex)
			{
				if (!SupressMessages)
				{
					MessageBox.Show(ex.Message, "Download has failed.", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
			}
			finally
			{
				stream?.Close();
				stream2?.Close();
				ftpWebResponse?.Close();
				if (pBar != null)
				{
					((Form)pBar).Close();
				}
			}
			if (flag)
			{
				if (gunzip)
				{
					Cursor.set_Current(Cursors.get_WaitCursor());
					FileInfo[] files = new DirectoryInfo(AppPath + "\\Downloaded files").GetFiles(FileName);
					foreach (FileInfo fileInfo in files)
					{
						using FileStream stream3 = fileInfo.OpenRead();
						string fullName = fileInfo.FullName;
						using FileStream destination = File.Create(fullName.Remove(fullName.Length - fileInfo.Extension.Length));
						using GZipStream gZipStream = new GZipStream(stream3, CompressionMode.Decompress);
						gZipStream.CopyTo(destination);
					}
					using (FileStream stream4 = new FileStream(AppPath + "\\Downloaded files\\" + FileName, FileMode.Open, FileAccess.Read))
					{
						using GZipStream gZipStream2 = new GZipStream(stream4, CompressionMode.Decompress);
						int num4 = FileName.IndexOf(".");
						if (num4 < 0)
						{
							num4 = FileName.Length;
						}
						using FileStream fileStream = new FileStream(FinalDestination + FileName.Substring(0, num4) + ".dat", FileMode.Create);
						byte[] buffer2 = new byte[1000];
						int num5 = 0;
						while ((num5 = gZipStream2.Read(buffer2, 0, 1000)) > 0)
						{
							fileStream.Write(buffer2, 0, num5);
						}
					}
					Cursor.set_Current(Cursors.get_Default());
				}
				if (unzip)
				{
					Cursor.set_Current(Cursors.get_WaitCursor());
					ZipFile.ExtractToDirectory(AppPath + "\\Downloaded files\\" + FileName, FinalDestination);
				}
				Cursor.set_Current(Cursors.get_Default());
				if (!SupressMessages)
				{
					MessageBox.Show(FileName + " successfully downloaded", "Success", (MessageBoxButtons)0);
				}
			}
			else if (CancelFlag)
			{
				MessageBox.Show(FileName + " - download cancelled", "Cancelled", (MessageBoxButtons)0);
			}
			return flag;
		}

		internal static bool UploadFTP(string FileToUpload_withPath, string DestinationFileName_withPath, string HostIP_noPath)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Unknown result type (might be due to invalid IL or missing references)
			int num = 2048;
			string userName = "Anonymous";
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				if ((int)MessageBox.Show("You must specify your Email address for anonymous FTP", "No email address", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
				{
					return false;
				}
				Defaults defaults = new Defaults();
				((Control)defaults.FTPPassword).Focus();
				((Form)defaults).ShowDialog();
			}
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				return false;
			}
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			try
			{
				FtpWebRequest obj = (FtpWebRequest)WebRequest.Create("ftp://" + HostIP_noPath + "/" + DestinationFileName_withPath);
				obj.Credentials = new NetworkCredential(userName, fTP_AnonymousPassword);
				obj.UseBinary = true;
				obj.UsePassive = true;
				obj.KeepAlive = true;
				obj.Proxy = null;
				obj.Method = "STOR";
				Stream requestStream = obj.GetRequestStream();
				FileStream fileStream = new FileStream(FileToUpload_withPath, FileMode.Open);
				byte[] buffer = new byte[num];
				int num2 = fileStream.Read(buffer, 0, num);
				try
				{
					while (num2 != 0)
					{
						requestStream.Write(buffer, 0, num2);
						num2 = fileStream.Read(buffer, 0, num);
					}
				}
				catch (Exception ex)
				{
					MessageBox.Show(ex.ToString(), "Upload Error");
				}
				fileStream.Close();
				requestStream.Close();
			}
			catch (Exception ex2)
			{
				MessageBox.Show(ex2.ToString(), "Upload Error");
			}
			return true;
		}

		public static void Download_EOP_current(bool SupressMessages)
		{
			//IL_029d: Unknown result type (might be due to invalid IL or missing references)
			int result;
			double result2;
			double result3;
			double result4;
			if (DownloadFTP(Settings.Default.EOPpost62_Server, Settings.Default.EOPpost62_File, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: false, SupressMessages: true))
			{
				BinaryWriter binaryWriter = new BinaryWriter(new FileStream(Downloads.EOPpresentFile, FileMode.Create, FileAccess.Write));
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\" + Settings.Default.EOPpost62_File))
				{
					do
					{
						string text = streamReader.ReadLine()!.PadRight(50);
						if (int.TryParse(text.Substring(12, 7), out result))
						{
							if (!double.TryParse(text.Substring(19, 11), out result2))
							{
								result2 = 0.0;
							}
							if (!double.TryParse(text.Substring(30, 11), out result3))
							{
								result3 = 0.0;
							}
							if (!double.TryParse(text.Substring(41, 12), out result4))
							{
								result4 = 0.0;
							}
							sbyte value = (sbyte)(100.0 * result2);
							sbyte value2 = (sbyte)(100.0 * result3);
							short value3 = (short)(1000.0 * result4);
							binaryWriter.Write(value);
							binaryWriter.Write(value2);
							binaryWriter.Write(value3);
						}
					}
					while (!streamReader.EndOfStream);
				}
				binaryWriter.Close();
			}
			http.DownloadHTTP("https://maia.usno.navy.mil", "ser7/finals2000A.data", Utilities.AppPath + "\\DownLoaded Files\\EOP_USNO_finals2000A.data", unzip: false, gunzip: false, SupressMessages);
			BinaryWriter binaryWriter2 = new BinaryWriter(new FileStream(Utilities.AppPath + "\\Resource Files\\EOP_2020plus.dat", FileMode.Create, FileAccess.Write));
			using (StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\EOP_USNO_finals2000A.data"))
			{
				do
				{
					string text = streamReader2.ReadLine()!.PadRight(70);
					if (text.Trim().Length < 20)
					{
						break;
					}
					int.TryParse(text.Substring(7, 5), out result);
					if (result >= 58849)
					{
						if (!double.TryParse(text.Substring(18, 7), out result2))
						{
							result2 = 0.0;
						}
						if (!double.TryParse(text.Substring(37, 7), out result3))
						{
							result3 = 0.0;
						}
						if (!double.TryParse(text.Substring(58, 7), out result4))
						{
							result4 = 0.0;
						}
						sbyte value = (sbyte)(100.0 * result2);
						sbyte value2 = (sbyte)(100.0 * result3);
						short value3 = (short)(1000.0 * result4);
						binaryWriter2.Write(value);
						binaryWriter2.Write(value2);
						binaryWriter2.Write(value3);
					}
				}
				while (!streamReader2.EndOfStream);
			}
			binaryWriter2.Close();
			if (!SupressMessages)
			{
				MessageBox.Show("Current Earth Orientation Parameters from  IERS and USNO have been converted");
			}
			Utilities.EOPdateSet = false;
		}

		public static void Download_EOP_Old(bool SupressMessages)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_017b: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("Downloading the 'pre-1962' data should not be needed\r\nDo you want to download it?", "Download pre-1962", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7 || !DownloadFTP(Settings.Default.EOPpre62_Server, Settings.Default.EOPpre62_File, Utilities.AppPath + "\\DownLoaded Files\\", unzip: false, gunzip: false, SupressMessages))
			{
				return;
			}
			BinaryWriter binaryWriter = new BinaryWriter(new FileStream(Downloads.EOPoldFile, FileMode.Create, FileAccess.Write));
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\" + Settings.Default.EOPpre62_File))
			{
				double result;
				do
				{
					string text = streamReader.ReadLine()!.PadRight(50);
					if (double.TryParse(text.Substring(0, 12), out result))
					{
						if (!double.TryParse(text.Substring(12, 10), out var result2))
						{
							result2 = 0.0;
						}
						if (!double.TryParse(text.Substring(22, 10), out var result3))
						{
							result3 = 0.0;
						}
						if (!double.TryParse(text.Substring(32, 11), out var result4))
						{
							result4 = 0.0;
						}
						sbyte value = (sbyte)(100.0 * result2);
						sbyte value2 = (sbyte)(100.0 * result3);
						short value3 = (short)(1000.0 * result4);
						binaryWriter.Write(value);
						binaryWriter.Write(value2);
						binaryWriter.Write(value3);
					}
				}
				while (!(result > 38000.0) && !streamReader.EndOfStream);
			}
			binaryWriter.Close();
			if (!SupressMessages)
			{
				MessageBox.Show("Past Earth Orientation Parameters have been converted");
			}
		}
	}
}
