using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Net;
using System.Net.Mail;
using System.Net.Security;
using System.Reflection;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult
{
	public class http
	{
		internal static List<string> SatNumber = new List<string>();

		internal static List<double> SatUncert = new List<double>();

		internal static string HorizonsSatUncerts = Utilities.AppPath + "\\Resource Files\\Sat_Uncertainties.csv";

		internal static bool CancelFlag;

		internal static bool HorizonsFailed = false;

		internal static DateTime HorizonsFirstFailTime;

		internal static bool DownloadHTTP(string httpAddress_WithoutFileName, string FileName, string FinalDestination, bool unzip, bool gunzip, bool ShowMessages)
		{
			return DownloadHTTP(httpAddress_WithoutFileName, FileName, FinalDestination, unzip, gunzip, ShowMessages, ShowProgressbar: true, "", "");
		}

		public static bool DownloadHTTP(string httpAddress_WithoutFileName, string FileName, string FinalDestination, bool unzip, bool gunzip, bool ShowMessages, bool ShowProgressbar)
		{
			return DownloadHTTP(httpAddress_WithoutFileName, FileName, FinalDestination, unzip, gunzip, ShowMessages, ShowProgressbar, "", "");
		}

		public static bool DownloadHTTP(string httpAddress_WithoutFileName, string FileName, string DownloadPathAndName_notZipGz, bool unzip, bool gunzip, bool ShowMessages, bool ShowProgressbar, string Extract_ONLY_ThisFile_1, string Extract_ONLY_ThisFile_2)
		{
			//IL_0370: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Unknown result type (might be due to invalid IL or missing references)
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			HttpWebResponse httpWebResponse = null;
			Stream stream = null;
			Stream stream2 = null;
			PBar pBar = null;
			string path = "";
			DateTime lastWriteTime = DateTime.Now;
			bool flag = false;
			string text = ((!(httpAddress_WithoutFileName.EndsWith("/") | httpAddress_WithoutFileName.EndsWith("="))) ? (httpAddress_WithoutFileName + "/" + FileName) : (httpAddress_WithoutFileName + FileName));
			try
			{
				HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create(text);
				if (text.Contains("usno.navy.mil"))
				{
					httpWebRequest.ServerCertificateValidationCallback = (RemoteCertificateValidationCallback)Delegate.Combine(httpWebRequest.ServerCertificateValidationCallback, (RemoteCertificateValidationCallback)((object sender, X509Certificate cert, X509Chain chain, SslPolicyErrors error) => true));
				}
				httpWebRequest.Method = "GET";
				httpWebRequest.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
				httpWebResponse = (HttpWebResponse)httpWebRequest.GetResponse();
				uint num = (uint)httpWebResponse.ContentLength;
				_ = 2000000000;
				lastWriteTime = httpWebResponse.LastModified;
				stream = httpWebResponse.GetResponseStream();
				path = ((!(unzip || gunzip)) ? DownloadPathAndName_notZipGz : (Utilities.AppPath + "\\Downloaded files\\" + FileName));
				if (((FileName.ToLower() == "future.dat") | (FileName.ToLower() == "futureall540.zip")) && File.Exists(path) && Settings.Default.PreserveFuture_dat)
				{
					string sourceFileName = Path.GetDirectoryName(DownloadPathAndName_notZipGz) + "\\" + Path.GetFileName(path)!.Replace("zip", "dat");
					DateTime dateTime = DateTime.Now.ToUniversalTime();
					string text2 = Path.GetDirectoryName(DownloadPathAndName_notZipGz) + "\\" + Path.GetFileNameWithoutExtension(path) + "_" + dateTime.Year + dateTime.Month.ToString().PadLeft(2, '0') + dateTime.Day.ToString().PadLeft(2, '0') + Path.GetExtension(path)!.Replace("zip", "dat");
					if (!File.Exists(text2))
					{
						File.Move(sourceFileName, text2);
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
				pBar.pBarFTP.set_Maximum(102);
				((Control)pBar).set_Text("Downloading " + text);
				((Form)pBar).set_StartPosition((FormStartPosition)1);
				((Form)pBar).set_TopMost(true);
				if (ShowProgressbar)
				{
					((Control)pBar).Show();
				}
				((Control)pBar).set_Top(10);
				((Control)pBar).set_Left(10);
				CancelFlag = false;
				byte[] buffer = new byte[4096];
				int num2 = 0;
				uint num3 = 0u;
				while ((num2 = stream.Read(buffer, 0, 4096)) > 0)
				{
					stream2.Write(buffer, 0, num2);
					num3 = (uint)stream2.Length;
					if (ShowProgressbar)
					{
						pBar.pBarFTP.set_Value((int)(100.0 * (double)num3 / (double)(num + 100)));
						pBar.Devents();
					}
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
				}
			}
			catch (Exception ex)
			{
				if (ShowMessages)
				{
					MessageBox.Show(ex.Message, "Download has failed.", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
			}
			finally
			{
				stream?.Close();
				stream2?.Close();
				httpWebResponse?.Close();
				if (pBar != null)
				{
					((Form)pBar).Close();
				}
			}
			if (flag)
			{
				File.SetLastWriteTime(path, lastWriteTime);
				if (gunzip)
				{
					Cursor.set_Current(Cursors.get_WaitCursor());
					FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Downloaded files").GetFiles(FileName);
					foreach (FileInfo fileInfo in files)
					{
						using FileStream stream3 = fileInfo.OpenRead();
						string fullName = fileInfo.FullName;
						using FileStream destination = File.Create(fullName.Remove(fullName.Length - fileInfo.Extension.Length));
						using GZipStream gZipStream = new GZipStream(stream3, CompressionMode.Decompress);
						gZipStream.CopyTo(destination);
					}
					Cursor.set_Current(Cursors.get_Default());
				}
				bool flag2 = true;
				if (unzip)
				{
					Cursor.set_Current(Cursors.get_WaitCursor());
					flag2 = Unzip.UnZip(Utilities.AppPath + "\\Downloaded files\\" + FileName, DownloadPathAndName_notZipGz, Extract_ONLY_ThisFile_1, Extract_ONLY_ThisFile_2);
				}
				Cursor.set_Current(Cursors.get_Default());
				if (!flag2)
				{
					MessageBox.Show(FileName + " was not unzipped, and the downmloaded .zip file was deleted. Any previous version of " + FileName + " of this file should still be presenrt for use", "Success", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
				else if (ShowMessages)
				{
					MessageBox.Show(FileName + " successfully downloaded", "Success", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
			}
			else if (CancelFlag)
			{
				MessageBox.Show(FileName + " - download cancelled", "Cancelled", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			return flag;
		}

		public static bool DownloadHTTP_string(string FullhttpAddress, out string Content)
		{
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			Stream stream = null;
			Content = "";
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create(FullhttpAddress);
			httpWebRequest.Method = "GET";
			httpWebRequest.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			try
			{
				HttpWebResponse obj = (HttpWebResponse)httpWebRequest.GetResponse();
				_ = obj.ContentLength;
				stream = obj.GetResponseStream();
				byte[] array = new byte[4096];
				Content = "";
				while (stream.Read(array, 0, 4096) > 0)
				{
					Content += Encoding.Default.GetString(array);
				}
			}
			catch
			{
				return false;
			}
			return true;
		}

		public static bool SaveHTTP_WebPage(string sourceAddress, string SavePathWithName)
		{
			WebClient webClient = new WebClient();
			try
			{
				webClient.DownloadFile(sourceAddress, SavePathWithName);
				return true;
			}
			catch
			{
				return false;
			}
		}

		public static void Download_Future(bool SupressMessages)
		{
			DownloadHTTP(Settings.Default.Future_Server, Settings.Default.FutureFile_xml.ToLower(), Utilities.AppPath + "\\Generated Files\\future.xml", unzip: false, gunzip: false, SupressMessages);
		}

		public static void Download_Future_ALL(bool SupressMessages)
		{
			DownloadHTTP(Settings.Default.FutureAll_Server, Settings.Default.FutureAll_File_XML, Utilities.AppPath + "\\Generated Files\\", unzip: true, gunzip: false, SupressMessages);
		}

		public static string Download_WebPage(string httpAddress)
		{
			//IL_002f: Unknown result type (might be due to invalid IL or missing references)
			WebClient webClient = new WebClient();
			try
			{
				return webClient.DownloadString(httpAddress);
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting Miriade data : \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return "";
			}
		}

		public static bool Check_ISAM_PageExists()
		{
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create("http://isam.astro.amu.edu.pl/home1.php?lang=en");
			httpWebRequest.Method = "GET";
			httpWebRequest.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			httpWebRequest.Timeout = 1000;
			try
			{
				if (((HttpWebResponse)httpWebRequest.GetResponse()).StatusCode == HttpStatusCode.OK)
				{
					return true;
				}
			}
			catch
			{
				return false;
			}
			return false;
		}

		public static bool IsISAM_UpToDate(out string ResponseText, out int CountFromISAM)
		{
			ResponseText = "";
			CountFromISAM = -1;
			int result = 0;
			if (File.Exists(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv") && new FileInfo(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv").Length > 3)
			{
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
				{
					if (!int.TryParse(streamReader.ReadLine()!.Substring(5), out result))
					{
						result = 0;
					}
				}
				if (result > 100000)
				{
					result = 0;
				}
			}
			if (!Utilities.InternetIsAvailable())
			{
				return result > 0;
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Getting Number of asteroids in ISAM");
			((Control)messageForm).Show();
			Application.DoEvents();
			HttpWebRequest obj = (HttpWebRequest)WebRequest.Create("http://isam.astro.amu.edu.pl/home1.php?lang=en");
			obj.Method = "GET";
			obj.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			using (HttpWebResponse httpWebResponse = (HttpWebResponse)obj.GetResponse())
			{
				using (Stream stream = httpWebResponse.GetResponseStream())
				{
					using StreamReader streamReader2 = new StreamReader(stream);
					ResponseText = streamReader2.ReadToEnd();
				}
				string value = "<option value=";
				int num = 0;
				do
				{
					num = ResponseText.IndexOf(value, num + 10);
					if (num > 0)
					{
						CountFromISAM++;
					}
				}
				while (num > 0);
			}
			((Form)messageForm).Close();
			return (result >= CountFromISAM) & (CountFromISAM > 0);
		}

		public static bool Get_ISAM_Asteroids(bool ForceUpdate)
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0080: Invalid comparison between Unknown and I4
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a1: Invalid comparison between Unknown and I4
			bool flag = false;
			bool flag2 = false;
			bool flag3 = false;
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("You are not connected to the internet. Retrieving the list of ISAM asteroids is only available if the internet is available.", "No internet", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return false;
			}
			int Num = 0;
			string ModsVers = "";
			string ResponseText = "";
			int num = 0;
			int CountFromISAM = 0;
			bool result = false;
			int i = 0;
			if (IsISAM_UpToDate(out ResponseText, out CountFromISAM))
			{
				Settings.Default.ISAM_DateLastChecked = DateTime.Now;
				if (!ForceUpdate)
				{
					return false;
				}
				if ((int)MessageBox.Show("The number of asteroids in the file of available ISAM asteroids matches the number of asteroids available on the ISAM database.\r\n\r\nThe only reason to update the file of available ISAM asteroids is the possibility of a change in the number of models available for each asteroid.\r\n\r\nA download of the currently available ISAM asteroid models will take many minutes.\r\n\r\nDo you want to continue with the download in any case?", "Long download time for ISAM models", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return false;
				}
			}
			else if ((int)MessageBox.Show("The number of asteroids in the file of available ISAM asteroids is different to the number of asteroids available on the ISAM database.\r\n\r\nYou need to update the file of available ISAM asteroids.\r\n\r\nA download of the currently available ISAM asteroid models will take many minutes.\r\n\r\nDo you want to continue with the download?", "Long download time for ISAM models", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			List<ISAMavailable> list = new List<ISAMavailable>();
			try
			{
				PBar pBar = new PBar();
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(CountFromISAM);
				((Control)pBar).set_Text("Retrieving the number of available models for each ISAM asteroid");
				((Control)pBar).Show();
				MessageForm messageForm = new MessageForm();
				((Control)messageForm).Show();
				int num2 = 0;
				int startIndex = 0;
				int num3;
				do
				{
					num3 = ResponseText.IndexOf("<option value=", startIndex);
					if (num3 < 0)
					{
						break;
					}
					int num4 = ResponseText.IndexOf(">", num3) + 1;
					startIndex = ResponseText.IndexOf("<", num4);
					if (num > 0)
					{
						Utilities.SetAnimatedCursor(ref i);
						string text = ResponseText.Substring(num3 + 14, num4 - num3 - 15);
						((Control)messageForm.label).set_Text("Getting model numbers for asteroid (" + text + ")");
						Application.DoEvents();
						GetISAMmodels_nonDAMIT_Versions(text, out Num, out ModsVers);
						if (Num > 0)
						{
							ISAMavailable iSAMavailable = new ISAMavailable();
							iSAMavailable.Line = text + "," + Num + "," + ModsVers;
							list.Add(iSAMavailable);
						}
						num2++;
						if (num2 % 5 == 0)
						{
							if (pBar.pBarFTP.get_Maximum() - num2 < 1)
							{
								ProgressBar pBarFTP = pBar.pBarFTP;
								pBarFTP.set_Maximum(pBarFTP.get_Maximum() + 100);
							}
							pBar.pBarFTP.set_Value(num2);
							Application.DoEvents();
						}
					}
					num++;
				}
				while (num3 > 0);
				for (int j = 0; j < list.Count; j++)
				{
					if (list[j].Number == 1)
					{
						flag = true;
					}
					if (list[j].Number == 4)
					{
						flag2 = true;
					}
					if (list[j].Number == 486958)
					{
						flag3 = true;
					}
				}
				int num5 = 0;
				if (!flag)
				{
					ISAMavailable iSAMavailable = new ISAMavailable();
					iSAMavailable.Number = 1;
					iSAMavailable.line = "1,1,110,2016-01-01";
					list.Add(iSAMavailable);
					num5++;
				}
				if (!flag2)
				{
					ISAMavailable iSAMavailable = new ISAMavailable();
					iSAMavailable.Number = 4;
					iSAMavailable.line = "4,1,110,2016-01-01";
					list.Add(iSAMavailable);
					num5++;
				}
				if (!flag3)
				{
					ISAMavailable iSAMavailable = new ISAMavailable();
					iSAMavailable.Number = 486958;
					iSAMavailable.line = "486958,1,110,2016-01-01";
					list.Add(iSAMavailable);
					num5++;
				}
				list.Sort();
				using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
				{
					streamWriter.WriteLine("Ast# " + list.Count);
					for (int k = 0; k < list.Count; k++)
					{
						streamWriter.WriteLine(list[k].Line);
					}
				}
				((Form)messageForm).Close();
				((Form)pBar).Close();
				Settings.Default.ISAM_DateLastChecked = DateTime.Now;
				if (num > 0)
				{
					result = true;
				}
			}
			catch
			{
			}
			Cursor.set_Current(Cursors.get_Default());
			return result;
		}

		internal static void GetISAMmodels_nonDAMIT_Versions(string AsteroidNumber, out int Num, out string ModsVers)
		{
			ModsVers = "";
			Num = 0;
			HttpWebRequest obj = (HttpWebRequest)WebRequest.Create("http://isam.astro.amu.edu.pl/home2.php?obiekt=" + AsteroidNumber + "&lang=en&strona=1&typ=Interactive%20mode&nr_modelu=1");
			obj.Method = "GET";
			obj.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			using HttpWebResponse httpWebResponse = (HttpWebResponse)obj.GetResponse();
			using Stream stream = httpWebResponse.GetResponseStream();
			using StreamReader streamReader = new StreamReader(stream);
			string text = streamReader.ReadToEnd();
			int num = -1;
			int num2 = -1;
			int num3 = -1;
			do
			{
				num = text.IndexOf("=\"nr_modelu", num + 1);
				if (num <= 0)
				{
					continue;
				}
				num = text.IndexOf("value=", num);
				num = text.IndexOf('"', num);
				num2 = text.IndexOf('"', num + 1);
				if (ModsVers.Length > 0)
				{
					ModsVers += ",";
				}
				if (int.Parse(text.Substring(num + 1, num2 - num - 1)) > 100)
				{
					Num++;
					ModsVers = ModsVers + text.Substring(num + 1, num2 - num - 1) + ",";
					for (int i = 0; i < 5; i++)
					{
						num = text.IndexOf("<td><center", num) + 5;
					}
					num2 = text.IndexOf(">", num) + 1;
					num3 = text.IndexOf("<", num2);
					if (num3 > 0)
					{
						string text2 = text.Substring(num2, num3 - num2).Replace("Version:", "").Replace("Inversion Techniques", "")
							.Replace(',', ';')
							.Replace("  ", " ")
							.Replace("  )", ")")
							.Replace(" )", ")")
							.Replace("(", "")
							.Replace(")", "")
							.Trim();
						ModsVers += text2;
					}
					else
					{
						ModsVers += " ";
					}
				}
			}
			while (num > 0);
		}

		public static void Get_ISAM_Asteroids_Updater()
		{
			if (Settings.Default.ISAM_DateLastChecked.AddMonths(3).CompareTo(DateTime.Now) >= 0 && Utilities.InternetIsAvailable() && Check_ISAM_PageExists())
			{
				if (!File.Exists(Downloads.ISAMFile))
				{
					Get_ISAM_Asteroids(ForceUpdate: false);
				}
				else
				{
					Get_ISAM_Asteroids(ForceUpdate: false);
				}
			}
		}

		public static bool UpdateListOfMiriadeBinaries(bool MustUpdateNow)
		{
			if (!Utilities.InternetIsAvailable())
			{
				return false;
			}
			if (!MustUpdateNow && DateTime.UtcNow.AddDays(-10.0) < Settings.Default.MiriadeBinaryUpdateDate)
			{
				return false;
			}
			try
			{
				string text = "";
				List<int> list = new List<int>();
				string[] array = Download_WebPage("https://ssp.imcce.fr/webservices/miriade/api/ephemsys.php?-get=systems&-from=Occult").Split(new char[1] { '\n' });
				for (int i = 1; i < array.Length; i++)
				{
					string[] array2 = array[i].Split(new char[1] { ',' });
					if (array2.Length > 1)
					{
						list.Add(int.Parse(array2[0]));
					}
				}
				list.Sort();
				for (int j = 0; j < list.Count; j++)
				{
					text = text + list[j] + " ";
				}
				Settings.Default.MiriadeAsteroids = text;
				Settings.Default.MiriadeBinaryUpdateDate = DateTime.UtcNow;
				return true;
			}
			catch
			{
				return false;
			}
		}

		public static bool GetInstallationDataFiles()
		{
			bool result = false;
			if (!Utilities.InternetIsAvailable())
			{
				return result;
			}
			result = true;
			result &= DownloadHTTP(Settings.Default.OccultServer, "InstallResources.zip", Utilities.AppPath + "\\Downloaded Files\\InstallResources.zip", unzip: false, gunzip: false, ShowMessages: false);
			return result & DownloadHTTP(Settings.Default.OccultServer, "InstallSites.zip", Utilities.AppPath + "\\Downloaded Files\\InstallSites.zip", unzip: false, gunzip: false, ShowMessages: false);
		}

		internal static void GetCometLastObservations()
		{
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			string text = "https://www.minorplanetcenter.net/iau/lists/LastCometObs.html";
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create(text);
			httpWebRequest.UserAgent = "Occult v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;
			try
			{
				using StreamReader streamReader = new StreamReader(((HttpWebResponse)httpWebRequest.GetResponse()).GetResponseStream());
				string text2 = streamReader.ReadToEnd();
				if (text2.Length > 1000)
				{
					int num = text2.IndexOf("Designation ");
					int num2 = text2.LastIndexOf("</pre><hr><p>");
					using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt");
					streamWriter.Write(text2.Substring(num, num2 - num));
					return;
				}
			}
			catch (WebException ex)
			{
				MessageBox.Show("Error accessing the web page for Dates of last Observations of Comets at\r\n" + text + "\r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		public static void GetAsteroidClasses()
		{
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_0019: Invalid comparison between Unknown and I4
			//IL_04e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("This function assumes you have updated the file of Binary Asteroids from the Maintenance tab, Edit Binary Asteroids. If you haven't updated this file recently, you should do this before proceeding.\r\n\r\nDo you want to continue?", "Check update of Binaries", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			List<AsteroidClass> list = new List<AsteroidClass>();
			string[] array = new string[10];
			string[] array2 = new string[10] { "Aten", "Apollo", "Amor", "Trojan (Mars)", "Trojan", "Trojan (Neptune)", "TNO", "Centaur", "PHA", "Binary" };
			string[] array3 = new string[9] { "https://www.minorplanetcenter.net/iau/lists/Atens.html", "https://www.minorplanetcenter.net/iau/lists/Apollos.html", "https://www.minorplanetcenter.net/iau/lists/Amors.html", "https://www.minorplanetcenter.net/iau/lists/MarsTrojans.html", "https://www.minorplanetcenter.net/iau/lists/JupiterTrojans.html", "https://www.minorplanetcenter.net/iau/lists/NeptuneTrojans.html", "https://www.minorplanetcenter.net/iau/lists/TNOs.html", "https://www.minorplanetcenter.net/iau/lists/Centaurs.html", "https://www.minorplanetcenter.net/iau/lists/PHAs.html" };
			PBar pBar = new PBar();
			pBar.pBarFTP.set_Maximum(array.Length);
			pBar.pBarFTP.set_Minimum(0);
			((Control)pBar).Show();
			for (int i = 0; i < array.Length - 1; i++)
			{
				pBar.pBarFTP.set_Value(i + 1);
				((Control)pBar).set_Text("Retrieving " + array2[i]);
				Application.DoEvents();
				array[i] = Download_WebPage(array3[i]);
			}
			((Form)pBar).Close();
			for (int j = 0; j < array.Length - 1; j++)
			{
				string[] array4 = array[j].Split(new string[2] { "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
				if (array4.Length < 200)
				{
					continue;
				}
				int num = 0;
				int num2;
				do
				{
					num2 = array4[num].IndexOf("Designation (and name)");
					num++;
				}
				while (num2 < 0);
				num++;
				do
				{
					num2 = array4[num].IndexOf("(");
					int num3 = array4[num].IndexOf(")");
					if (num3 > num2 && num3 < 12)
					{
						AsteroidClass asteroidClass = new AsteroidClass();
						asteroidClass.Number = int.Parse(array4[num].Substring(num2 + 1, num3 - num2 - 1));
						asteroidClass.AstClass = array2[j];
						list.Add(asteroidClass);
					}
					else if (array4[num].Contains("<"))
					{
						break;
					}
					num++;
				}
				while (num < array4.Length);
			}
			string path = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Binaries.csv";
			if (File.Exists(path))
			{
				using StreamReader streamReader = new StreamReader(path);
				streamReader.ReadLine();
				do
				{
					string[] array5 = streamReader.ReadLine()!.Split(new char[1] { ',' });
					AsteroidClass asteroidClass = new AsteroidClass();
					asteroidClass.Number = int.Parse(array5[0]);
					asteroidClass.AstClass = array2[9];
					list.Add(asteroidClass);
				}
				while (!streamReader.EndOfStream);
			}
			string path2 = Utilities.AppPath + "\\Resource Files\\BinaryAsteroids.csv";
			if (File.Exists(path2))
			{
				using StreamReader streamReader2 = new StreamReader(path2);
				streamReader2.ReadLine();
				do
				{
					string[] array6 = streamReader2.ReadLine()!.Split(new char[1] { ',' });
					if (array6[0].Length != 0)
					{
						AsteroidClass asteroidClass = new AsteroidClass();
						asteroidClass.Number = int.Parse(array6[0]);
						asteroidClass.AstClass = array2[9];
						list.Add(asteroidClass);
					}
				}
				while (!streamReader2.EndOfStream);
			}
			list.Sort();
			for (int num4 = list.Count - 2; num4 >= 0; num4--)
			{
				if (list[num4].Number == list[num4 + 1].Number)
				{
					if (list[num4 + 1].AstClass == "PHA")
					{
						list[num4].AstClass = list[num4].AstClass + " + PHA";
					}
					else if (list[num4].AstClass == "PHA")
					{
						list[num4].AstClass = "PHA + " + list[num4 + 1].AstClass;
					}
					else
					{
						list[num4].AstClass = list[num4].AstClass + " + " + list[num4 + 1].AstClass;
					}
					list[num4].AstClass = list[num4].AstClass.Replace("Binary + Binary", "Binary");
					list.RemoveAt(num4 + 1);
				}
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Resource Files/AsteroidClasses.csv"))
			{
				for (int k = 0; k < list.Count; k++)
				{
					streamWriter.WriteLine(list[k].ToString());
				}
			}
			MessageBox.Show("File of asteroid classes has been created\r\n\r\nTo make this active, you MUST Convert the asteroid elements.", "Successful download", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			((Form)new ConvertAstorb_etc(1)).ShowDialog();
		}

		internal static void Download_Comet(bool SupressMessages)
		{
			string comet_Server = Settings.Default.Comet_Server;
			string comet_file = Settings.Default.Comet_file;
			string cometFile = Downloads.CometFile;
			DownloadHTTP(comet_Server, comet_file, cometFile, unzip: false, gunzip: false, SupressMessages);
			GetCometLastObservations();
		}

		internal static void DownloadAsteroidClassFile()
		{
			//IL_0046: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Unknown result type (might be due to invalid IL or missing references)
			if (Utilities.InternetIsAvailable())
			{
				string occultServer = Settings.Default.OccultServer;
				string fileName = "AsteroidClasses.zip";
				string finalDestination = Utilities.AppPath + "\\Resource Files\\";
				DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
				MessageBox.Show("File of asteroid classes has been updated\r\n\r\nTo make this active, you MUST Convert the asteroid elements.", "Successful download", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				((Form)new ConvertAstorb_etc(1)).ShowDialog();
			}
		}

		internal static void DownloadConstellationsFile()
		{
			if (Utilities.InternetIsAvailable())
			{
				string occultServer = Settings.Default.OccultServer;
				string fileName = "Constellations.zip";
				string finalDestination = Utilities.AppPath + "\\Resource Files\\";
				DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static bool DownloadBadHipStarsFile()
		{
			bool result = false;
			if (!Utilities.InternetIsAvailable())
			{
				return result;
			}
			string occultServer = Settings.Default.OccultServer;
			string fileName = "Gaia_ED3_BadHipStars.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia";
			Cursor.set_Current(Cursors.get_WaitCursor());
			result = DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			Cursor.set_Current(Cursors.get_Default());
			return result;
		}

		internal static bool DownloadUCAC4IndexFile()
		{
			bool result = false;
			if (!Utilities.InternetIsAvailable())
			{
				return result;
			}
			string occultServer = Settings.Default.OccultServer;
			string fileName = "U4_Gaia14.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia";
			Cursor.set_Current(Cursors.get_WaitCursor());
			result = DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			Cursor.set_Current(Cursors.get_Default());
			return result;
		}

		internal static string Get_LightCurve_EmailAddress()
		{
			string path = Utilities.AppPath + "\\Resource Files\\addresses.txt";
			if (File.Exists(path))
			{
				using StreamReader streamReader = new StreamReader(path);
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					if (text.ToUpper().Substring(0, 6) == "<LIGHT")
					{
						int num = text.IndexOf(">");
						if ((num > 1) & (num < text.Length - 1))
						{
							return text.Substring(num + 1);
						}
						return "";
					}
				}
			}
			return "";
		}

		internal static void Email_LightCurveReports()
		{
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_006f: Invalid comparison between Unknown and I4
			//IL_0265: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Unknown result type (might be due to invalid IL or missing references)
			//IL_031a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.AppPath + "\\Observations\\LightCurves";
			string text2 = Utilities.AppPath + "\\Observations\\LightCurves\\Reported";
			string lightCurve_EmailAddress = Get_LightCurve_EmailAddress();
			if (lightCurve_EmailAddress.Length < 4)
			{
				MessageBox.Show("No valid address to email the Light Curve.\r\n\r\nPlease go to the Downloads page and download the latest 'Reporting addresses for occultations'", "No valid address", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_InitialDirectory(text);
			val.set_Multiselect(true);
			((FileDialog)val).set_Title("Select the Light curve files to report by Email");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				int num = ((FileDialog)val).get_FileNames().Length;
				string text3 = "";
				string subject = "Light curve report";
				string body = num + " light curve reports attached\r\n\r\n";
				string to = lightCurve_EmailAddress;
				string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
				string fTP_AnonymousPassword2 = Settings.Default.FTP_AnonymousPassword;
				string eMailServerName = Settings.Default.EMailServerName;
				MailMessage mailMessage = new MailMessage(fTP_AnonymousPassword2, to, subject, body);
				mailMessage.Bcc.Add(fTP_AnonymousPassword);
				string[] array = Settings.Default.LightCurve_CC_Addresses.Split(new char[1] { ';' });
				for (int i = 0; i < array.GetUpperBound(0); i++)
				{
					if (array[i].Contains("@"))
					{
						mailMessage.CC.Add(array[i].Trim());
					}
				}
				for (int j = 0; j < num; j++)
				{
					mailMessage.Attachments.Add(new Attachment(((FileDialog)val).get_FileNames()[j].ToString()));
				}
				SmtpClient smtpClient = new SmtpClient(eMailServerName);
				if (Settings.Default.Email.Length > 0)
				{
					smtpClient.Credentials = new NetworkCredential(Settings.Default.EmailUser, Settings.Default.Email);
				}
				smtpClient.EnableSsl = Settings.Default.EmailUseSSL;
				if (!int.TryParse(Settings.Default.EmailPort, out var result))
				{
					result = -1;
				}
				if (result >= 0)
				{
					smtpClient.Port = result;
				}
				try
				{
					smtpClient.Send(mailMessage);
				}
				catch (SmtpFailedRecipientsException ex)
				{
					for (int k = 0; k < ex.InnerExceptions.Length; k++)
					{
						text3 = text3 + "Failed to deliver message to " + ex.FailedRecipient![k] + "\r\n";
					}
				}
				catch (SmtpException ex2)
				{
					text3 = text3 + ex2.Message + "\r\n\r\n";
				}
				mailMessage.Dispose();
				if (text3.Length > 5)
				{
					MessageBox.Show(text3, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				if (Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0].ToString())!.ToLower() != text.ToLower())
				{
					MessageBox.Show(num + " light curves have been emailed", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				for (int l = 0; l < num; l++)
				{
					string fileName = Path.GetFileName(((FileDialog)val).get_FileNames()[l].ToString());
					File.Move(text + "\\" + fileName, text2 + "\\" + fileName);
				}
				MessageBox.Show(num + " light curves have been emailed, and the files moved to the '\\Observations\\LightCurves\\Reported' folder", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				MessageBox.Show("The selected light curves have not been emailed", "Not emailed", (MessageBoxButtons)0, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		internal static void GetSAOcatalogue()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "SAO.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			}
		}

		public static void GetUBSCcatalogue()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "UBSC.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static void GetGaiaDoublesFile()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia_Doubles.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static void GetGaiaBinariesFile()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "Gaia_Binaries.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Gaia\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static void GetAAVSOFile()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "AAVSOindex.zip";
			string finalDestination = Utilities.AppPath + "\\Downloaded Files\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static void Download_AsteroidDias(bool SupressMessages)
		{
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			string occultServer = Settings.Default.OccultServer;
			string text = "AsteroidDiameters.zip";
			string fileName = "AsteroidDiametersAll.zip";
			string fileName2 = "AsteroidDias_Indiv.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, !SupressMessages);
				DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, !SupressMessages);
				DownloadHTTP(occultServer, fileName2, finalDestination, unzip: true, gunzip: false, !SupressMessages);
				MessageBox.Show("IMPORTANT:\r\nTo ensure the updated the diameters of asteroids are used\r\nin all circumstances, you need to update your file of \r\nasteroid elements.\r\n\r\nYou will now be taken to the relevant page.", "Need to update elements", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		public static void GetAsteroidMagnitudeFile()
		{
			string occultServer = Settings.Default.OccultServer;
			string text = "AsteroidMagnitudes.zip";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\";
			if (!((occultServer.Trim().Length == 0) | (text.Trim().Length == 0)))
			{
				DownloadHTTP(occultServer, text, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			}
		}

		public static void GetEarth2014()
		{
			string text = "https://ddfe.curtin.edu.au/models/Earth2014/data_1min/topo_grids";
			string text2 = "Earth2014.SUR2014.1min.geod.bin";
			string finalDestination = Utilities.AppPath + "\\Resource Files\\Earth2014.SUR2014.1min.geod.bin";
			if (!((text.Trim().Length == 0) | (text2.Trim().Length == 0)))
			{
				DownloadHTTP(text, text2, finalDestination, unzip: false, gunzip: false, ShowMessages: true);
			}
		}

		public static string GetHorizonsElements_TDBtime(string AsteroidNumber, int EpochYear, int EpochMonth, int EpochDay, double EpochHour, ref AsteroidElements AE)
		{
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_069a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0759: Unknown result type (might be due to invalid IL or missing references)
			//IL_0891: Unknown result type (might be due to invalid IL or missing references)
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(60.0)) < 0)
			{
				return "";
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS orbital elements download :  " + AsteroidNumber.ToString());
			((Control)messageForm).Show();
			if (AsteroidNumber.EndsWith("p"))
			{
				AsteroidNumber = AsteroidNumber.ToUpper();
			}
			if (AsteroidNumber.Contains("/"))
			{
				AsteroidNumber = AsteroidNumber.ToUpper();
				if ((AsteroidNumber.Substring(0, 2) == "P/") | (AsteroidNumber.Substring(0, 2) == "C/"))
				{
					int num = AsteroidNumber.IndexOf("(");
					if (num > 2)
					{
						AsteroidNumber = AsteroidNumber.Substring(0, num - 1).Trim();
					}
				}
				else if (AsteroidNumber.Contains("P/"))
				{
					int num2 = AsteroidNumber.IndexOf("P/");
					AsteroidNumber = AsteroidNumber.Substring(0, num2 + 1);
				}
				else if (AsteroidNumber.Contains("P-"))
				{
					int num3 = AsteroidNumber.IndexOf("P-");
					int num4 = AsteroidNumber.IndexOf("/", num3);
					if (num4 > num3)
					{
						AsteroidNumber = AsteroidNumber.Substring(0, num4);
					}
				}
				else if (AsteroidNumber.Contains("I/"))
				{
					AsteroidNumber = AsteroidNumber[..AsteroidNumber.IndexOf("I/")];
				}
			}
			Application.DoEvents();
			if (EpochHour >= 24.0)
			{
				EpochHour -= 24.0;
				EpochDay++;
			}
			Utilities.Date_from_JD(Utilities.JD_from_Date(EpochYear, EpochMonth, (double)EpochDay + 1.01), out var Year, out var Month, out var day);
			string text = "";
			string text2 = "";
			string text3 = Utilities.DEGtoDMS(EpochHour, 2, 1, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text4 = EpochYear.ToString().Trim() + "-" + Utilities.ShortMonths[EpochMonth].ToUpper() + "-" + EpochDay.ToString().Trim() + " " + text3;
			string text5 = Year.ToString().Trim() + "-" + Utilities.ShortMonths[Month].ToUpper() + "-" + ((int)day).ToString().Trim() + " 0:0";
			string requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + AsteroidNumber.ToString() + "%3B%27&CENTER='500@10'&MAKE_EPHEM='YES'&EPHEM_TYPE='ELEMENTS'&START_TIME='" + text4 + "TDB'&STOP_TIME='" + text5 + "'&STEP_SIZE='2%20d'&OUT_UNITS='AU-D'&CSV_FORMAT='NO'";
			string requestUriString2 = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + AsteroidNumber.ToString() + "%3B%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text4 + "UT'&STOP_TIME='" + text5 + "'&STEP_SIZE='2%20d'&QUANTITIES='37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
			try
			{
				ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs\r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFailed = true;
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return "";
			}
			if (text.Contains("No matches found"))
			{
				MessageBox.Show("The object " + AsteroidNumber.ToString() + "\r\n was not found in Horizons", "Object not in Horizons", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				((Form)messageForm).Close();
				return "";
			}
			try
			{
				ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
				HttpWebRequest obj2 = (HttpWebRequest)WebRequest.Create(requestUriString2);
				obj2.Method = "GET";
				using WebResponse webResponse2 = obj2.GetResponse();
				using (Stream stream2 = webResponse2.GetResponseStream())
				{
					using StreamReader streamReader2 = new StreamReader(stream2);
					text2 = streamReader2.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex2)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex2.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFailed = true;
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return "";
			}
			string text6 = "";
			if (text.IndexOf("Matching small-bodies:") > 0)
			{
				List<string> list = new List<string>();
				int num5 = 0;
				int num6 = 0;
				int startIndex = text.IndexOf("-----");
				num5 = text.IndexOf("\n", startIndex) + 1;
				do
				{
					if (num6 > 10)
					{
						num5 = num6 + 1;
					}
					num6 = text.IndexOf("\n", num5);
					string text7 = text.Substring(num5, num6 - num5);
					if (text7.Length <= 5)
					{
						break;
					}
					list.Add(text7);
				}
				while (num6 - num5 > 10);
				if (list.Count > 0)
				{
					text6 = list[list.Count - 1].Trim().Split(new char[1] { ' ' })[0].Trim();
				}
				requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text6 + "%3B%27&CENTER='500@10'&MAKE_EPHEM='YES'&EPHEM_TYPE='ELEMENTS'&START_TIME='" + text4 + "TDB'&STOP_TIME='" + text5 + "'&STEP_SIZE='2%20d'&OUT_UNITS='AU-D'&CSV_FORMAT='NO'";
				requestUriString2 = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text6 + "%3B%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text4 + "UT'&STOP_TIME='" + text5 + "'&STEP_SIZE='2%20d'&QUANTITIES='37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
				try
				{
					ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
					HttpWebRequest obj3 = (HttpWebRequest)WebRequest.Create(requestUriString);
					obj3.Method = "GET";
					using WebResponse webResponse3 = obj3.GetResponse();
					using Stream stream3 = webResponse3.GetResponseStream();
					using StreamReader streamReader3 = new StreamReader(stream3);
					text = streamReader3.ReadToEnd();
				}
				catch (Exception ex3)
				{
					MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs" + ex3.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					HorizonsFailed = true;
					HorizonsFirstFailTime = DateTime.Now;
					((Form)messageForm).Close();
					return "";
				}
				try
				{
					ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
					HttpWebRequest obj4 = (HttpWebRequest)WebRequest.Create(requestUriString2);
					obj4.Method = "GET";
					using WebResponse webResponse4 = obj4.GetResponse();
					using (Stream stream4 = webResponse4.GetResponseStream())
					{
						using StreamReader streamReader4 = new StreamReader(stream4);
						text2 = streamReader4.ReadToEnd();
					}
					HorizonsFailed = false;
				}
				catch (Exception ex4)
				{
					MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex4.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					HorizonsFailed = true;
					HorizonsFirstFailTime = DateTime.Now;
					((Form)messageForm).Close();
					return "";
				}
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_ElementFile.txt"))
			{
				streamWriter.Write(text);
			}
			using (StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_EphemerisUncertValues.txt"))
			{
				streamWriter2.Write(text2);
			}
			double num7 = 0.0;
			double num8 = 0.15;
			double num9 = 5.0;
			double Diameter = 0.0;
			double DiameterUncertainty = 0.0;
			bool flag = false;
			string Source = "";
			List<AsteroidElements> list2 = new List<AsteroidElements>();
			list2.Clear();
			int num10 = text.IndexOf("$$SOE");
			if ((num10 < 0) | (text.Length < num10 + 365))
			{
				using (StreamWriter streamWriter3 = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_ErroneousElementFile.txt"))
				{
					streamWriter3.Write(text);
				}
				MessageBox.Show("The file of orbital elements downloaded from Horizons is not valid.\r\n\r\nA copy of the download has been written to the\r\n'Downloaded Files' subdirectory with the name\r\n\r\n'Horizons_ErroneousElementFile.txt'\r\n\r\nHorizons will be skipped for next 20 secs", "Incorrect copied section", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFailed = true;
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return "";
			}
			int num11 = text.IndexOf("Soln.date:");
			num11 = text.IndexOf(" ", num11 + 1);
			int num12 = text.IndexOf("_", num11 + 1) - num11;
			string orbitDate = text.Substring(num11, num12).Trim();
			string orbitSource = "";
			int num13 = text.IndexOf("soln ref.=");
			if (num13 > 0)
			{
				num13 = text.IndexOf("#", num13);
				if (num13 > 0)
				{
					num12 = text.IndexOf(",", num13) - num13;
				}
				if (num12 > 0 && num12 < 9)
				{
					orbitSource = text.Substring(num13 - 3, num12 + 3).Trim();
				}
			}
			int startIndex2 = text.IndexOf("Target body name:");
			int num14 = text.IndexOf(":", startIndex2) + 1;
			int num15 = text.IndexOf("{", startIndex2) - 1;
			string text8 = text.Substring(num14, num15 - num14).Trim();
			string text9;
			int result;
			if (text8.Contains("/"))
			{
				text9 = text8;
				result = 0;
			}
			else
			{
				int num16 = text8.IndexOf(" ");
				int.TryParse(text8.Substring(0, num16).Trim(), out result);
				int num17 = text8.IndexOf("(");
				int num18 = text8.IndexOf(")");
				text9 = ((num17 < 0) ? text8.Substring(num16 + 1).Trim() : ((num17 - num16 <= 1) ? text8.Substring(num17 + 1, num18 - num17 - 1).Trim() : text8.Substring(num16 + 1, num17 - num16 - 1).Trim()));
			}
			double.TryParse(text.Substring(num10 + 6, 17), out var result2);
			Utilities.Date_from_JD(result2, out var Year2, out var Month2, out var day2);
			num13 = text.IndexOf("EC=", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 21), out var result3);
			num13 = text.IndexOf("IN=", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 21), out var result4);
			num13 = text.IndexOf("OM=", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 21), out var result5);
			num13 = text.IndexOf("W =", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 21), out var result6);
			num13 = text.IndexOf("A =", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 22), out var result7);
			num13 = text.IndexOf("MA=", num10);
			if (num13 < 0)
			{
				return "";
			}
			double.TryParse(text.Substring(num13 + 4, 21), out var result8);
			if (result3 > 0.97)
			{
				num13 = text.IndexOf("Tp=", num10);
				if (num13 < 0)
				{
					return "";
				}
				double.TryParse(text.Substring(num13 + 4, 17), out result2);
				Utilities.Date_from_JD(result2, out Year2, out Month2, out day2);
				result8 = 0.0;
				num13 = text.IndexOf("QR=", num10);
				if (num13 < 0)
				{
					result7 *= 1.0 - result3;
				}
				else
				{
					double.TryParse(text.Substring(num13 + 4, 21), out result7);
				}
			}
			num7 = 0.0;
			num8 = 0.15;
			num9 = 5.0;
			int num19 = text.IndexOf("Initial");
			if (num19 < 0)
			{
				num19 = 0;
			}
			num13 = text.IndexOf(" H=", num19);
			if (num13 > 0)
			{
				flag = true;
				double.TryParse(text.Substring(num13 + 4, 5), out num7);
				num13 = text.IndexOf("G=", num19);
				if (num13 < 0)
				{
					num8 = 0.15;
				}
				else
				{
					double.TryParse(text.Substring(num13 + 3, 4), out num8);
				}
				if (num8 == 0.0)
				{
					num8 = 0.15;
				}
				num9 = 5.0;
			}
			if (num13 < 0 || num7 == 0.0)
			{
				num8 = 0.0;
				num13 = text.IndexOf(" M2=", num19);
				if (num13 > 0)
				{
					double.TryParse(text.Substring(num13 + 4, 6), out num7);
					if (num7 > 0.0)
					{
						num13 = text.IndexOf(" k2=", num13);
						double.TryParse(text.Substring(num13 + 4, 6), out num9);
						if (num9 == 0.0)
						{
							num9 = 5.0;
						}
					}
				}
				if (num13 < 0 || num7 == 0.0)
				{
					num13 = text.IndexOf(" M1=", num19 - 40);
					if (num13 < 0)
					{
						num13 = text.IndexOf(" M1=", num19);
					}
					if (num13 > 0)
					{
						double.TryParse(text.Substring(num13 + 4, 6), out num7);
						if (num7 > 0.0)
						{
							num13 = text.IndexOf(" k1=", num13);
							double.TryParse(text.Substring(num13 + 4, 6), out num9);
							if (num9 == 0.0)
							{
								num9 = 5.0;
							}
						}
						else
						{
							num7 = 15.0;
							num9 = 5.0;
						}
					}
					else
					{
						num7 = 15.0;
						num9 = 5.0;
					}
				}
			}
			bool flag2 = false;
			if (int.TryParse(AsteroidNumber, out var result9))
			{
				flag2 = Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(result9, out Diameter, out DiameterUncertainty, out Source);
			}
			if (Source == "6" || Diameter == 0.0 || !flag2)
			{
				num13 = text.IndexOf("RAD=", num19);
				double result10 = 0.0;
				if (text.Substring(num13 + 5, 8).Contains("km"))
				{
					num13 = text.IndexOf("RAD=", num13 + 10);
				}
				if (num13 > 0)
				{
					double.TryParse(text.Substring(num13 + 5, 8), out result10);
				}
				if (result10 <= 0.0)
				{
					if (Source != "6")
					{
						if (flag)
						{
							Diameter = Math.Pow(10.0, 3.52 - 0.2 * num7);
							if (Diameter < 0.1)
							{
								Diameter = 0.1;
							}
							DiameterUncertainty = Diameter / 10.0;
							Source = "6";
						}
						else
						{
							Diameter = 10.0;
							DiameterUncertainty = 2.0;
							Source = "6";
						}
					}
				}
				else
				{
					Diameter = 2.0 * result10;
					if (Diameter < 0.1)
					{
						Diameter = 0.1;
					}
					DiameterUncertainty = Diameter / 10.0;
					AE.Dia_Source = "9";
				}
			}
			num13 = text2.IndexOf(" Date__(UT)__HR:MN");
			int num20 = text2.IndexOf("\n", num13);
			string text10 = text2.Substring(num13, num20 - num13);
			int startIndex3 = text10.IndexOf("SMAA_3sig");
			int startIndex4 = text10.IndexOf("SMIA_3sig");
			int num21 = text10.IndexOf("Theta") - 3;
			int num22 = text10.IndexOf("Area_3sig");
			int startIndex5 = text10.IndexOf("POS_3sigma");
			num13 = text2.IndexOf("$$SOE");
			num13 = text2.IndexOf("\n", num13) + 1;
			num20 = text2.IndexOf("\n", num13);
			if (num13 < 0 || num20 < 0 || num20 < num13)
			{
				return "";
			}
			string text11 = text2.Substring(num13, num20 - num13);
			double.TryParse(text11.Substring(startIndex3, 9), out var result11);
			double.TryParse(text11.Substring(startIndex4, 9), out var result12);
			double.TryParse(text11.Substring(num21, num22 - num21), out var result13);
			double.TryParse(text11.Substring(startIndex5), out var result14);
			AE.IDNumber = result;
			AE.IDName = text9;
			AE.Meananomaly = result8;
			AE.EpochYear = Year2;
			AE.EpochMonth = Month2;
			AE.EpochDay = day2;
			AE.Perihelion = result6;
			AE.Node = result5;
			AE.I = result4;
			AE.E = result3;
			AE.A = result7;
			AE.H0 = num7;
			if (result3 > 0.97)
			{
				AE.Meananomaly = 0.0;
			}
			AE.G_phaseCoeff = num8;
			AE.LogR_Coeff = num9;
			AE.Diameter_Mean = Diameter;
			AE.DiameterUncertainty = DiameterUncertainty;
			AE.Dia_Source = Source;
			AE.PeakEphemUncert = result14 / 3.0;
			ConvertAstorb_etc convertAstorb_etc = new ConvertAstorb_etc(0);
			AE.Num_Rings = AsteroidRings_All.GetNumberOfRings(result);
			if (BinaryAsteroids.BinElements.Count < 1)
			{
				BinaryAsteroids.Fill_AllAsteroids();
			}
			ConvertAstorb_etc.BinaryRecNum = 0;
			AE.Num_Moons = convertAstorb_etc.GetMoons(result, text9);
			AE.OrbitSource = orbitSource;
			AE.OrbitDate = orbitDate;
			if (result > 0)
			{
				AE.AsteroidClass = AsteroidClassList.ClassOfAnAsteroid(result);
			}
			else
			{
				AE.AsteroidClass = "";
			}
			AE.ErrorMajor = result11 / 3.0;
			AE.ErrorMinor = result12 / 3.0;
			AE.ErrorPA = 90.0 - result13;
			list2.Add(AE);
			((Form)messageForm).Close();
			return list2[0].CSVString();
		}

		public static bool GetHorizonsSatelliteApparentEphemeris(int Planet, int Moon, double jd, bool UseTTnotUT, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double Uncertainty)
		{
			//IL_0332: Unknown result type (might be due to invalid IL or missing references)
			//IL_037d: Unknown result type (might be due to invalid IL or missing references)
			RA = new double[4];
			Dec = new double[4];
			Delta = new double[4];
			EphemSource = "Error";
			Uncertainty = 0.0;
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(20.0)) < 0)
			{
				return false;
			}
			string text = Planet + Moon.ToString().PadLeft(2, '0');
			string text2 = Satellites.SatelliteName(Planet, Moon);
			int num = text2.IndexOf("(");
			if (num > 0)
			{
				text2 = text2.Substring(0, num - 1).Trim();
			}
			text2 = Utilities.Planets[Planet] + " " + Utilities.Roman_from_Numeral(Moon) + " " + text2;
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS download :  " + text2);
			((Control)messageForm).Show();
			Application.DoEvents();
			Utilities.Date_from_JD(jd, out var Year, out var Month, out var day);
			int num2 = (int)day;
			string text3 = Utilities.DEGtoDMS((day - (double)num2) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text4 = Year.ToString().Trim() + "-" + Utilities.ShortMonths[Month].ToUpper() + "-" + num2.ToString().Trim() + " " + text3;
			Utilities.Date_from_JD(jd + 0.1250125, out var Year2, out var Month2, out var day2);
			int num3 = (int)day2;
			string text5 = Utilities.DEGtoDMS((day2 - (double)num3) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text6 = Year2.ToString().Trim() + "-" + Utilities.ShortMonths[Month2].ToUpper() + "-" + num3.ToString().Trim() + " " + text5;
			string text7 = "";
			string text8 = "UT";
			if (UseTTnotUT)
			{
				text8 = "TT";
			}
			string requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text + "%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text4 + text8 + "'&STOP_TIME='" + text6 + "'&STEP_SIZE='60%20min'&QUANTITIES='2,20,37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text7 = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return false;
			}
			if (text7.Contains("Inform JPL"))
			{
				MessageBox.Show("The file at Horizons to generate the ephemeris is not\r\navailable, with the following report being received.\r\n________________________________\r\n\r\n" + text7 + "________________________________\r\n\r\nYou can continue with your search, but this satellite\r\nwill not have an Ephemeris from Horizons", "Missing file at Horizons", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return false;
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_EphemerisFile.txt"))
			{
				streamWriter.Write(text7);
			}
			int num4 = text7.IndexOf(" Date__(" + text8 + ")__HR:MN");
			int num5 = text7.IndexOf("\n", num4);
			string text9 = text7.Substring(num4, num5 - num4);
			int num6 = text9.IndexOf("R.A._");
			int num7 = text9.IndexOf("delta");
			num4 = text7.IndexOf("$$SOE");
			string text10;
			for (int i = 0; i < 4; i++)
			{
				num4 = text7.IndexOf("\n", num4) + 1;
				num5 = text7.IndexOf("\n", num4);
				text10 = text7.Substring(num4, num5 - num4);
				double.TryParse(text10.Substring(num6, 13), out var result);
				RA[i] = result / (180.0 / Math.PI);
				int num8 = text10.IndexOf(" ", num6 + 20);
				double.TryParse(text10.Substring(num6 + 14, num8 - num6 - 14), out result);
				Dec[i] = result / (180.0 / Math.PI);
				double.TryParse(text10.Substring(num8, num7 - num8 + 4), out result);
				Delta[i] = result;
			}
			num4 = text7.IndexOf("Target");
			num5 = text7.IndexOf("\n", num4);
			text10 = text7.Substring(num4, num5 - num4);
			num4 = text10.IndexOf("{so");
			num4 = text10.IndexOf(":", num4);
			num5 = text10.LastIndexOf("}");
			EphemSource = text10.Substring(num4 + 1, num5 - num4 - 1).Trim();
			Uncertainty = GetHorizonsSatUncert(text);
			((Form)messageForm).Close();
			return true;
		}

		public static bool GetHorizonsCometApparentEphemeris(string CometID, double jd, bool UseTTnotUT, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double Uncertainty, out double Diameter, out double T_Mag, out double N_Mag)
		{
			Uncertainty = 1000.0;
			if (GetHorizonsCometApparentEphemeris_J2000_or_Apparent(CometID, jd, 1.0, UseTTnotUT, IsJ2000NotApparent: false, out RA, out Dec, out Delta, out EphemSource, out var ErrorMajor, out var ErrorMinor, out var _, out Diameter, out T_Mag, out N_Mag))
			{
				Uncertainty = Math.Sqrt(ErrorMajor * ErrorMajor + ErrorMinor * ErrorMinor);
				return true;
			}
			return false;
		}

		public static bool GetHorizonsCometApparentEphemeris_J2000_or_Apparent(string CometID, double jd, double StepInHours, bool UseTTnotUT, bool IsJ2000NotApparent, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double ErrorMajor, out double ErrorMinor, out double ErrorPA, out double Diameter, out double T_Mag, out double N_Mag)
		{
			//IL_0330: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Unknown result type (might be due to invalid IL or missing references)
			RA = new double[4];
			Dec = new double[4];
			Delta = new double[4];
			EphemSource = "Error";
			ErrorMajor = (ErrorMinor = (ErrorPA = 0.0));
			Diameter = 1.0;
			double result = 15.0;
			T_Mag = 30.0;
			N_Mag = 30.0;
			double result2 = 0.2;
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(20.0)) < 0)
			{
				return false;
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS download :  " + CometID);
			((Control)messageForm).Show();
			Application.DoEvents();
			Utilities.Date_from_JD(jd, out var Year, out var Month, out var day);
			int num = (int)day;
			string text = Utilities.DEGtoDMS((day - (double)num) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text2 = Year.ToString().Trim() + "-" + Utilities.ShortMonths[Month].ToUpper() + "-" + num.ToString().Trim() + " " + text;
			Utilities.Date_from_JD(jd + 3.0003 * StepInHours / 24.0, out var Year2, out var Month2, out var day2);
			int num2 = (int)day2;
			string text3 = Utilities.DEGtoDMS((day2 - (double)num2) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text4 = Year2.ToString().Trim() + "-" + Utilities.ShortMonths[Month2].ToUpper() + "-" + num2.ToString().Trim() + " " + text3;
			string text5 = "";
			string text6 = "UT";
			if (UseTTnotUT)
			{
				text6 = "TT";
			}
			string text7 = "2";
			if (IsJ2000NotApparent)
			{
				text7 = "1";
			}
			string requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + CometID + "%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text2 + text6 + "'&STOP_TIME='" + text4 + "'&STEP_SIZE='60%20min'&QUANTITIES='" + text7 + ",9,20,37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text5 = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return false;
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_EphemerisCometFileInit.txt"))
			{
				streamWriter.Write(text5);
			}
			string text8 = "";
			if (text5.IndexOf("Matching small-bodies:") > 0)
			{
				List<string> list = new List<string>();
				int num3 = 0;
				int num4 = 0;
				int startIndex = text5.IndexOf("-----");
				num3 = text5.IndexOf("\n", startIndex) + 1;
				do
				{
					if (num4 > 10)
					{
						num3 = num4 + 1;
					}
					num4 = text5.IndexOf("\n", num3);
					string text9 = text5.Substring(num3, num4 - num3);
					if (text9.Length <= 5)
					{
						break;
					}
					list.Add(text9);
				}
				while (num4 - num3 > 10);
				if (list.Count > 0)
				{
					text8 = list[list.Count - 1].Trim().Split(new char[1] { ' ' })[0].Trim();
				}
				requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text8 + "%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text2 + text6 + "'&STOP_TIME='" + text4 + "'&STEP_SIZE='60%20min'&QUANTITIES='" + text7 + ",9,20,37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
				try
				{
					HttpWebRequest obj2 = (HttpWebRequest)WebRequest.Create(requestUriString);
					obj2.Method = "GET";
					using WebResponse webResponse2 = obj2.GetResponse();
					using (Stream stream2 = webResponse2.GetResponseStream())
					{
						using StreamReader streamReader2 = new StreamReader(stream2);
						text5 = streamReader2.ReadToEnd();
					}
					HorizonsFailed = false;
				}
				catch (Exception ex2)
				{
					MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex2.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					HorizonsFirstFailTime = DateTime.Now;
					((Form)messageForm).Close();
					return false;
				}
			}
			using (StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_EphemerisCometFile.txt"))
			{
				streamWriter2.Write(text5);
			}
			int num5 = text5.IndexOf(" Date__(" + text6 + ")__HR:MN");
			int num6 = text5.IndexOf("\n", num5);
			string text10 = text5.Substring(num5, num6 - num5);
			int num7 = text10.IndexOf("R.A._");
			int num8 = text10.IndexOf("T-mag") + 4;
			int num9 = text10.IndexOf("N-mag") + 4;
			int num10 = text10.IndexOf("delta");
			int startIndex2 = text10.IndexOf("SMAA_3sig");
			int startIndex3 = text10.IndexOf("SMIA_3sig");
			int num11 = text10.IndexOf("Area_3sig");
			int num12 = text10.IndexOf("Theta") - 3;
			num5 = text5.IndexOf("Target");
			num6 = text5.IndexOf("\n", num5);
			string text11 = text5.Substring(num5, num6 - num5);
			num5 = text11.IndexOf("{so");
			num5 = text11.IndexOf(":", num5);
			num6 = text11.LastIndexOf("}");
			EphemSource = text11.Substring(num5 + 1, num6 - num5 - 1).Trim();
			num5 = text5.IndexOf("$$SOE");
			for (int i = 0; i < 4; i++)
			{
				num5 = text5.IndexOf("\n", num5) + 1;
				num6 = text5.IndexOf("\n", num5);
				text11 = text5.Substring(num5, num6 - num5);
				double.TryParse(text11.Substring(num7, 13), out var result3);
				RA[i] = result3 / (180.0 / Math.PI);
				int num13 = text11.IndexOf(" ", num7 + 20);
				double.TryParse(text11.Substring(num7 + 14, num13 - num7 - 14), out result3);
				Dec[i] = result3 / (180.0 / Math.PI);
				double.TryParse(text11.Substring(num9 + 1, num10 - num9), out result3);
				Delta[i] = result3;
				if (i == 1)
				{
					T_Mag = (N_Mag = 20.0);
					if (num8 > 4)
					{
						double.TryParse(text11.Substring(num8 - 6, 7), out T_Mag);
					}
					if (num9 > 4)
					{
						double.TryParse(text11.Substring(num9 - 6, 7), out N_Mag);
					}
					double.TryParse(text11.Substring(startIndex2, 9), out ErrorMajor);
					double.TryParse(text11.Substring(startIndex3, 9), out ErrorMinor);
					double.TryParse(text11.Substring(num12, num11 - num12), out ErrorPA);
					ErrorMajor /= 3.0;
					ErrorMinor /= 3.0;
					ErrorPA = 90.0 - ErrorPA;
				}
			}
			num5 = text5.IndexOf(" M2=");
			if (num5 < 0)
			{
				result = 19.1;
			}
			else
			{
				double.TryParse(text5.Substring(num5 + 4, 6), out result);
			}
			num5 = text5.IndexOf("RAD= km");
			if (num5 > 0)
			{
				num5 = text5.IndexOf("RAD=", num5 + 10);
			}
			if (num5 > 0)
			{
				double.TryParse(text5.Substring(num5 + 5, 8), out result2);
			}
			if (result2 <= 0.0)
			{
				if (result > 0.0)
				{
					Diameter = Math.Pow(10.0, 3.52 - 0.2 * result);
					if (Diameter < 0.1)
					{
						Diameter = 0.1;
					}
				}
				else
				{
					Diameter = 10.0;
				}
			}
			else
			{
				Diameter = 2.0 * result2;
			}
			((Form)messageForm).Close();
			return true;
		}

		public static bool GetHorizonsAsteroidJ2000Ephemeris(string AsteroidNumber, double jd, double StepInHours, bool UseTTnotUT, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double ErrorMajor, out double ErrorMinor, out double ErrorPA)
		{
			double Mag;
			return GetHorizonsAsteroidJ2000Ephemeris(AsteroidNumber, jd, StepInHours, UseTTnotUT, UseOldHorizonsOrbit: false, "", out RA, out Dec, out Delta, out EphemSource, out ErrorMajor, out ErrorMinor, out ErrorPA, out Mag);
		}

		public static bool GetHorizonsAsteroidJ2000Ephemeris(string AsteroidNumber, double jd, double StepInHours, bool UseTTnotUT, bool UseOldHorizonsOrbit, string HorizonsOrbit, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double ErrorMajor, out double ErrorMinor, out double ErrorPA)
		{
			double Mag;
			return GetHorizonsAsteroidJ2000Ephemeris(AsteroidNumber, jd, StepInHours, UseTTnotUT, UseOldHorizonsOrbit: false, "", out RA, out Dec, out Delta, out EphemSource, out ErrorMajor, out ErrorMinor, out ErrorPA, out Mag);
		}

		public static bool GetHorizonsAsteroidJ2000Ephemeris(string AsteroidNumber, double jd, double StepInHours, bool UseTTnotUT, bool UseOldHorizonsOrbit, string HorizonsOrbit, out double[] RA, out double[] Dec, out double[] Delta, out string EphemSource, out double ErrorMajor, out double ErrorMinor, out double ErrorPA, out double Mag)
		{
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			RA = new double[4];
			Dec = new double[4];
			Delta = new double[4];
			EphemSource = "Error";
			ErrorMajor = (ErrorMinor = (ErrorPA = (Mag = 0.0)));
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(20.0)) < 0)
			{
				return false;
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS Ephemeris download :  " + AsteroidNumber);
			((Control)messageForm).Show();
			Application.DoEvents();
			if (AsteroidNumber.EndsWith("p"))
			{
				AsteroidNumber = AsteroidNumber.ToUpper();
			}
			if (AsteroidNumber.Contains("/"))
			{
				AsteroidNumber = AsteroidNumber.ToUpper();
				if ((AsteroidNumber.Substring(0, 2) == "P/") | (AsteroidNumber.Substring(0, 2) == "C/"))
				{
					int num = AsteroidNumber.IndexOf("(");
					if (num > 2)
					{
						AsteroidNumber = AsteroidNumber.Substring(0, num - 1).Trim();
					}
				}
				else if (AsteroidNumber.Contains("P/"))
				{
					int num2 = AsteroidNumber.IndexOf("P/");
					AsteroidNumber = AsteroidNumber.Substring(0, num2 + 1);
				}
				else if (AsteroidNumber.Contains("P-"))
				{
					int num3 = AsteroidNumber.IndexOf("P-");
					int num4 = AsteroidNumber.IndexOf("/", num3);
					if (num4 > num3)
					{
						AsteroidNumber = AsteroidNumber.Substring(0, num4);
					}
				}
				else if (AsteroidNumber.Contains("I/"))
				{
					AsteroidNumber = AsteroidNumber[..AsteroidNumber.IndexOf("I/")];
				}
			}
			Utilities.Date_from_JD(jd, out var Year, out var Month, out var day);
			int num5 = (int)day;
			string text = Utilities.DEGtoDMS((day - (double)num5) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text2 = Year.ToString().Trim() + "-" + Utilities.ShortMonths[Month].ToUpper() + "-" + num5.ToString().Trim() + " " + text;
			Utilities.Date_from_JD(jd + 3.00001 * StepInHours / 24.0, out var Year2, out var Month2, out var day2);
			int num6 = (int)day2;
			string text3 = Utilities.DEGtoDMS((day2 - (double)num6) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text4 = Year2.ToString().Trim() + "-" + Utilities.ShortMonths[Month2].ToUpper() + "-" + num6.ToString().Trim() + " " + text3;
			string text5 = "";
			string text6 = "UT";
			if (UseTTnotUT)
			{
				text6 = "TT";
			}
			string text7 = string.Format("{0,1:f0}", 60.0 * StepInHours);
			string text8 = (UseOldHorizonsOrbit ? ("https://ssd.jpl.nasa.gov/api/horizons.api?format=text&" + HorizonsOrbit + "CENTER='500@399'&START_TIME='" + text2 + text6 + "'&STOP_TIME='" + text4 + "'&STEP_SIZE='" + text7 + "%20min'&QUANTITIES='1,9,20,37,38'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&EXTRA_PREC='YES'&OBJ_DATA='YES'") : ("https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + AsteroidNumber + "%3B%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text2 + text6 + "'&STOP_TIME='" + text4 + "'&STEP_SIZE='" + text7 + "%20min'&QUANTITIES='1,9,20,37,38'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&EXTRA_PREC='YES'&OBJ_DATA='YES'"));
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(text8);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text5 = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return false;
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Downloaded Files/Horizons_EphemerisFile.txt"))
			{
				streamWriter.Write(text5);
			}
			int num7 = text5.IndexOf(" Date__(" + text6 + ")__HR:MN");
			if (num7 < 0)
			{
				((Form)messageForm).Close();
				return false;
			}
			int num8 = text5.IndexOf("\n", num7);
			string text9 = text5.Substring(num7, num8 - num7);
			int num9 = text9.IndexOf("R.A._");
			text9.IndexOf("delta");
			int startIndex = text9.IndexOf("S-brt") + 6;
			int startIndex2 = text9.IndexOf("SMAA_3sig");
			int startIndex3 = text9.IndexOf("SMIA_3sig");
			int num10 = text9.IndexOf("Theta") - 3;
			int num11 = text9.IndexOf("Area_3sig");
			int num12 = text9.IndexOf("APmag") - 1;
			if (num12 < 0)
			{
				num12 = text9.IndexOf("T-mag") - 1;
			}
			num7 = text5.IndexOf("$$SOE");
			string text10;
			for (int i = 0; i < 4; i++)
			{
				num7 = text5.IndexOf("\n", num7) + 1;
				num8 = text5.IndexOf("\n", num7);
				text10 = text5.Substring(num7, num8 - num7);
				if (i == 0)
				{
					double.TryParse(text10.Substring(startIndex2, 9), out ErrorMajor);
					double.TryParse(text10.Substring(startIndex3, 9), out ErrorMinor);
					double.TryParse(text10.Substring(num10, num11 - num10), out ErrorPA);
					double.TryParse(text10.Substring(num12, 6), out Mag);
					ErrorMajor /= 3.0;
					ErrorMinor /= 3.0;
					ErrorPA = 90.0 - ErrorPA;
				}
				double.TryParse(text10.Substring(num9, 13), out var result);
				RA[i] = result / (180.0 / Math.PI);
				int num13 = text10.IndexOf(" ", num9 + 20);
				double.TryParse(text10.Substring(num9 + 14, num13 - num9 - 14), out result);
				Dec[i] = result / (180.0 / Math.PI);
				double.TryParse(text10.Substring(startIndex, 12), out result);
				Delta[i] = result;
			}
			num7 = text5.IndexOf("Target");
			num8 = text5.IndexOf("\n", num7);
			text10 = text5.Substring(num7, num8 - num7);
			num7 = text10.IndexOf("{so");
			num7 = text10.IndexOf(":", num7);
			num8 = text10.LastIndexOf("}");
			EphemSource = text10.Substring(num7 + 1, num8 - num7 - 1).Trim();
			if (UseOldHorizonsOrbit)
			{
				int num14 = text8.IndexOf("&OBJECT=");
				if (num14 > 0)
				{
					int num15 = text8.IndexOf("'", num14);
					int num16 = text8.IndexOf("'", num15 + 2);
					EphemSource = text8.Substring(num15 + 1, num16 - num15 - 1);
				}
			}
			((Form)messageForm).Close();
			return true;
		}

		public static void GetHorizons_Asteroid_or_Satellite_ApparentPosition(bool GetAsteroid, int AsteroidNumber, int Planet, int Moon, double jd, bool UseTTnotUT, out double RA, out double Dec, out double Delta, out string SatName, out float SatDia, out string EphemSource, out double Uncertainty)
		{
			//IL_035f: Unknown result type (might be due to invalid IL or missing references)
			RA = (Dec = (Delta = (Uncertainty = (SatDia = 0f))));
			SatName = (EphemSource = "");
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(20.0)) < 0)
			{
				return;
			}
			string text = AsteroidNumber + "%3B";
			string text2 = Planet + Moon.ToString().PadLeft(2, '0');
			string text3 = Satellites.SatelliteName(Planet, Moon);
			int num = text3.IndexOf("(");
			if (num > 0)
			{
				text3 = text3.Substring(0, num - 1).Trim();
			}
			text3 = Utilities.Planets[Planet] + " " + Utilities.Roman_from_Numeral(Moon) + " " + text3;
			string text4 = ((!GetAsteroid) ? text2 : text);
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS download :  " + text3);
			((Control)messageForm).Show();
			Application.DoEvents();
			Utilities.Date_from_JD(jd, out var Year, out var Month, out var day);
			int num2 = (int)day;
			string text5 = Utilities.DEGtoDMS((day - (double)num2) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text6 = Year.ToString().Trim() + "-" + Utilities.ShortMonths[Month].ToUpper() + "-" + num2.ToString().Trim() + " " + text5;
			Utilities.Date_from_JD(jd + 1.1574074074074073E-05, out var Year2, out var Month2, out var day2);
			int num3 = (int)day2;
			string text7 = Utilities.DEGtoDMS((day2 - (double)num3) * 24.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: false).Trim().Replace("  ", " ")
				.Replace(" ", ":");
			string text8 = Year2.ToString().Trim() + "-" + Utilities.ShortMonths[Month2].ToUpper() + "-" + num3.ToString().Trim() + " " + text7;
			string text9 = "";
			string text10 = "UT";
			if (UseTTnotUT)
			{
				text10 = "TT";
			}
			string requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text4 + "%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text6 + text10 + "'&STOP_TIME='" + text8 + "'&STEP_SIZE='60%20min'&QUANTITIES='2,20,37,38'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text9 = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFailed = true;
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return;
			}
			int num4 = text9.IndexOf(" Date__(" + text10 + ")__HR:MN");
			int num5 = text9.IndexOf("\n", num4);
			string text11 = text9.Substring(num4, num5 - num4);
			int num6 = text11.IndexOf("R.A._");
			int num7 = text11.IndexOf("delta");
			num4 = text9.IndexOf("$$SOE");
			num4 = text9.IndexOf("\n", num4) + 1;
			num5 = text9.IndexOf("\n", num4);
			string text12 = text9.Substring(num4, num5 - num4);
			double.TryParse(text12.Substring(num6, 13), out var result);
			RA = result / (180.0 / Math.PI);
			int num8 = text12.IndexOf(" ", num6 + 20);
			double.TryParse(text12.Substring(num6 + 14, num8 - num6 - 14), out result);
			Dec = result / (180.0 / Math.PI);
			double.TryParse(text12.Substring(num8, num7 - num8 + 4), out result);
			Delta = result;
			num4 = text9.IndexOf("Target");
			num5 = text9.IndexOf("\n", num4);
			text12 = text9.Substring(num4, num5 - num4);
			num4 = text12.IndexOf("{so");
			num4 = text12.IndexOf(":", num4);
			num5 = text12.LastIndexOf("}");
			EphemSource = text12.Substring(num4 + 1, num5 - num4 - 1).Trim();
			num4 = text12.IndexOf("e:");
			num5 = text12.IndexOf("(", num4);
			SatName = text12.Substring(num4 + 2, num5 - num4 - 2).Trim();
			num4 = text9.IndexOf("Target radii");
			num5 = text9.IndexOf("\n", num4);
			text12 = text9.Substring(num4, num5 - num4);
			num4 = text12.IndexOf(":");
			num5 = text12.IndexOf("x");
			if (num5 < 0)
			{
				num5 = text12.IndexOf("km", num4);
			}
			if (num5 > num4)
			{
				float.TryParse(text12.Substring(num4 + 1, num5 - num4 - 1).Trim(), out SatDia);
				SatDia *= 2f;
			}
			else
			{
				SatDia = 1f;
			}
			Uncertainty = GetHorizonsSatUncert(text2);
			((Form)messageForm).Close();
		}

		public static bool GetHorizons_Asteroid_30hr_ApparentEphemeris_Less2_Plus27(int AsteroidNumber, int Year, int Month, int Day, bool UseTTnotUT, out double[] RA, out double[] Dec, out double[] Delta, out double MagV, out double ErrorMajor, out double ErrorMinor, out double ErrorPA)
		{
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			RA = new double[30];
			Dec = new double[30];
			Delta = new double[30];
			MagV = 25.0;
			ErrorMajor = (ErrorMinor = (ErrorPA = 0.0));
			string text = AsteroidNumber + "%3B";
			if (HorizonsFailed && DateTime.Compare(DateTime.Now, HorizonsFirstFailTime.AddSeconds(20.0)) < 0)
			{
				return false;
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("HORIZONS download :  " + AsteroidNumber);
			((Control)messageForm).Show();
			Application.DoEvents();
			Utilities.Date_from_JD(Utilities.JD_from_Date(Year, Month, Day) - 1.0, out var Year2, out var Month2, out var day);
			Utilities.Date_from_JD(Utilities.JD_from_Date(Year, Month, Day) + 1.0, out var Year3, out var Month3, out var day2);
			string text2 = Year2.ToString().Trim() + "-" + Utilities.ShortMonths[Month2].ToUpper() + "-" + day.ToString().Trim() + " 22:0";
			string text3 = Year3.ToString().Trim() + "-" + Utilities.ShortMonths[Month3].ToUpper() + "-" + day2.ToString().Trim() + " 3:0";
			string text4 = "";
			string text5 = "UT";
			if (UseTTnotUT)
			{
				text5 = "TT";
			}
			string requestUriString = "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND=%27" + text + "%3B%27&MAKE_EPHEM='YES'&EPHEM_TYPE='OBSERVER'&CENTER='500@399'&START_TIME='" + text2 + text5 + "'&STOP_TIME='" + text3 + "'&STEP_SIZE='60%20min'&QUANTITIES='2,20,37,38,9'&CAL_FORMAT='CAL'&TIME_DIGITS='SECONDS'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='YES'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='NO'&OBJ_DATA='NO'&APPARENT='AIRLESS'";
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Method = "GET";
				using WebResponse webResponse = obj.GetResponse();
				using (Stream stream = webResponse.GetResponseStream())
				{
					using StreamReader streamReader = new StreamReader(stream);
					text4 = streamReader.ReadToEnd();
				}
				HorizonsFailed = false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data from Horizons.\r\nHorizons will be skipped for next 20 secs \r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				HorizonsFailed = true;
				HorizonsFirstFailTime = DateTime.Now;
				((Form)messageForm).Close();
				return false;
			}
			int num = text4.IndexOf(" Date__(" + text5 + ")__HR:MN");
			int num2 = text4.IndexOf("\n", num);
			string text6 = text4.Substring(num, num2 - num);
			int num3 = text6.IndexOf("R.A._");
			int num4 = text6.IndexOf("delta");
			int num5 = text6.IndexOf("APmag") + 4;
			int num6 = text6.IndexOf("SMAA_3sig") + 8;
			int num7 = text6.IndexOf("SMIA_3sig") + 8;
			int num8 = text6.IndexOf("Theta") + 4;
			num = text4.IndexOf("$$SOE");
			for (int i = 0; i < 30; i++)
			{
				num = text4.IndexOf("\n", num) + 1;
				num2 = text4.IndexOf("\n", num);
				string text7 = text4.Substring(num, num2 - num);
				double.TryParse(text7.Substring(num3, 13), out var result);
				RA[i] = result / (180.0 / Math.PI);
				int num9 = text7.IndexOf(" ", num3 + 20);
				double.TryParse(text7.Substring(num3 + 14, num9 - num3 - 14), out result);
				Dec[i] = result / (180.0 / Math.PI);
				double.TryParse(text7.Substring(num9, num4 - num9 + 4), out result);
				Delta[i] = result;
				if (i == 14)
				{
					int num10 = text7.LastIndexOf(" ", num6);
					double.TryParse(text7.Substring(num10, num6 - num10), out result);
					ErrorMajor = result / 3.0;
					num10 = text7.LastIndexOf(" ", num7);
					double.TryParse(text7.Substring(num10, num7 - num10), out result);
					ErrorMinor = result / 3.0;
					num10 = text7.LastIndexOf(" ", num8);
					double.TryParse(text7.Substring(num10, num8 - num10), out result);
					ErrorPA = 90.0 - result;
					num10 = text7.LastIndexOf(" ", num5);
					double.TryParse(text7.Substring(num10, num5 - num10), out result);
					MagV = result;
				}
			}
			((Form)messageForm).Close();
			return true;
		}

		public static bool WriteSatelliteUncertainties()
		{
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Downloading HORIZONS satellite uncertainties");
			((Control)messageForm).Show();
			Application.DoEvents();
			string text = Download_WebPage("https://ssd.jpl.nasa.gov/?sat_ephem");
			if (text.Length < 1000)
			{
				return false;
			}
			string text2 = "";
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\Sat_Uncertainties.csv"))
			{
				for (int i = 4; i < 10; i++)
				{
					for (int j = 1; j < 20 && !(i == 4 && j > 2) && !(i == 8 && j > 13) && !(i == 9 && j > 5); j++)
					{
						string text3 = i + j.ToString().PadLeft(2, '0');
						int startIndex = text.IndexOf("right>" + text3 + " ");
						int startIndex2 = text.IndexOf("<A HREF", startIndex);
						startIndex = text.LastIndexOf("left>", startIndex2);
						startIndex = text.IndexOf(">", startIndex);
						startIndex2 = text.IndexOf("</", startIndex + 1);
						text2 = text.Substring(startIndex + 1, startIndex2 - startIndex).Replace("<", "");
						if (!double.TryParse(text2, out var _))
						{
							text2 = "100";
						}
						streamWriter.WriteLine(text3 + "," + text2);
					}
				}
			}
			return true;
		}

		public static double GetHorizonsSatUncert(string SatID)
		{
			if (!File.Exists(HorizonsSatUncerts))
			{
				if (!Utilities.InternetIsAvailable())
				{
					return 50.0;
				}
				string occultServer = Settings.Default.OccultServer;
				string text = "sat_uncertainties.csv";
				string finalDestination = Utilities.AppPath + "\\Resource Files\\" + text;
				DownloadHTTP(occultServer, text, finalDestination, unzip: false, gunzip: false, ShowMessages: false);
			}
			if (SatNumber.Count < 20)
			{
				using StreamReader streamReader = new StreamReader(HorizonsSatUncerts);
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					SatNumber.Add(array[0]);
					double.TryParse(array[1], out var result);
					if (result == 0.0)
					{
						result = 50.0;
					}
					SatUncert.Add(result);
				}
				while (!streamReader.EndOfStream);
			}
			for (int i = 0; i < SatNumber.Count; i++)
			{
				if (SatNumber[i] == SatID)
				{
					return SatUncert[i];
				}
			}
			return 50.0;
		}

		internal static bool Get_GoogleEarth_Elevations(ref List<GoogleMapsCoords> GMC)
		{
			//IL_01c7: Unknown result type (might be due to invalid IL or missing references)
			bool result = true;
			int num = 10;
			MessageForm messageForm = new MessageForm();
			((Control)messageForm).Show();
			for (int i = 0; i < GMC.Count; i++)
			{
				if (i % num != 0)
				{
					continue;
				}
				string text = "";
				result = true;
				string text2 = "";
				int num2 = 0;
				for (int j = i; j < GMC.Count; j++)
				{
					text2 = ((j != i) ? (text2 + string.Format("|{0,1:f4},{1,1:f4}", GMC[j].Latitude, GMC[j].Longitude)) : string.Format("{0,1:f4},{1,1:f4}", GMC[j].Latitude, GMC[j].Longitude));
					num2++;
					if (num2 == num)
					{
						break;
					}
				}
				if (text2.Length <= 0)
				{
					break;
				}
				((Control)messageForm.label).set_Text($"Downloading Sites elevations, sites {GMC[i].SeqNum} to {GMC[i + num2 - 1].SeqNum}");
				Application.DoEvents();
				string requestUriString = "https://maps.googleapis.com/maps/api/elevation/xml?locations=" + text2 + "&key=" + Settings.Default.GoogleMaps_API_key;
				try
				{
					ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
					HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
					obj.Method = "GET";
					using WebResponse webResponse = obj.GetResponse();
					using (Stream stream = webResponse.GetResponseStream())
					{
						using StreamReader streamReader = new StreamReader(stream);
						text = streamReader.ReadToEnd();
					}
					result = true;
				}
				catch (Exception ex)
				{
					MessageBox.Show("Error requesting data from Google Earth.\r\n\r\n" + ex.Message, "Error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					result = false;
				}
				string[] array = text.Split(new string[3] { "\r\n", "\r", "\n" }, StringSplitOptions.None);
				int num3 = i;
				int num4 = 0;
				int num5 = 0;
				for (int k = 0; k < array.Length; k++)
				{
					if (array[k].Contains("<elevation>"))
					{
						num4 = array[k].IndexOf(">");
						num5 = array[k].IndexOf("<", num4);
						GMC[num3].GE_Altitude = (int)double.Parse(array[k].Substring(num4 + 1, num5 - num4 - 1));
						GMC[num3].Resolution = 0.0;
						if (array[k + 1].Contains("<resolution>"))
						{
							num4 = array[k + 1].IndexOf(">");
							num5 = array[k + 1].IndexOf("<", num4);
							GMC[num3].Resolution = (int)double.Parse(array[k + 1].Substring(num4 + 1, num5 - num4 - 1));
						}
						num3++;
						if (num3 % num >= num2)
						{
							break;
						}
					}
				}
				Thread.Sleep(100);
			}
			((Form)messageForm).Close();
			return result;
		}
	}
}
