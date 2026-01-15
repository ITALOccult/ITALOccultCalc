using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Net;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.File_Actions
{
	public class Create_JPL_DE_EphemerisFile : Form
	{
		internal static int OpeningTab = 0;

		private static string SourceFileFolder;

		private static string SourceHeader;

		private static int SourceBlockLength = 26873;

		private static bool NoNutations = false;

		private static int NCoeff = 0;

		private static string HeaderStartLine = "";

		private static string HeaderEndLine = "";

		private static string SourcePath;

		private static string Version = "";

		private static bool FilesAreContinuous = true;

		private static string DownloadVersion = "";

		private static string ProposedDEFile = "";

		private static string ProposedDefaultFile = "";

		internal static List<DESourceFiles> Sources = new List<DESourceFiles>();

		private DESourceFiles SourceFile;

		private IContainer components;

		private Button cmdSelectDEfolder;

		private Label lblSource;

		private Label lblHeader;

		private Button cmdCreateDEfile;

		private ProgressBar pBarBlock;

		private Panel panelPBar;

		private Label label2;

		private Label label4;

		private Label lblContinuity;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private TabControl tabControl1;

		private TabPage tabDownload;

		private TabPage tabConvert;

		private ListBox lstVersions;

		private Button cmdListVersions;

		private Label label5;

		private Label lblFiles;

		private Button cmdDownload;

		private Label label6;

		private Label lblFile;

		private Panel panelProcessing;

		private ListView listViewDownload;

		private ListView listViewFiles;

		private Label lblFileProcessing;

		private ToolTip toolTip1;

		private Label label7;

		private Label label3;

		private Label label1;

		private Label label8;

		private TabPage tabDefaults;

		private TabPage tabSetTemporary;

		private Label label9;

		private ListView listViewDEfiles;

		private Label LBLCurrentVersion;

		private Label label10;

		private Label lblLabel11;

		private Label lblDEproposed;

		private Button cmdUseSelectDEfile;

		private Label label11;

		private Button cmdClose;

		private Label label12;

		private ListView listViewDEfilesForDefaults;

		private GroupBox grpSetDE_Long;

		private Label label14;

		private GroupBox grpSetDEEphemeris;

		private Label label13;

		private Label lblCurrentDE_Long;

		private Label lblCurrentDE_Ephem;

		private Label lblProposedDefault;

		private Button cmdSetDE_Long;

		private Button cmdSetDE_Ephemeris;

		private Label label15;

		private Button cmdCancelDownload;

		public Create_JPL_DE_EphemerisFile()
		{
			InitializeComponent();
		}

		private void Create_JPL_DE_EphemerisFile_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)cmdCancelDownload).set_Location(((Control)cmdDownload).get_Location());
			SourceFileFolder = Utilities.AppPath + "\\DownLoaded Files\\DE_SourceFiles";
			tabControl1.set_SelectedIndex(OpeningTab);
		}

		private void cmdCreateDEfile_Click(object sender, EventArgs e)
		{
			if (Sources.Count >= 1)
			{
				CreateFile();
			}
		}

		private void CreateFile()
		{
			double[] array = new double[1021];
			((Control)panelPBar).set_Visible(true);
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\DE" + Version + ".bin", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			StreamWriter streamWriter = new StreamWriter(new FileStream(SourcePath + "\\" + Version + ".log", FileMode.Create, FileAccess.Write));
			using (StreamReader streamReader = new StreamReader(SourceHeader))
			{
				string text = streamReader.ReadLine();
				text = streamReader.ReadLine();
				text = streamReader.ReadLine();
				text = streamReader.ReadLine();
				char[] chars = streamReader.ReadLine()!.PadRight(84).ToCharArray();
				binaryWriter.Write(chars, 0, 84);
				chars = HeaderStartLine.PadRight(84).ToCharArray();
				binaryWriter.Write(chars, 0, 84);
				chars = HeaderEndLine.PadRight(84).ToCharArray();
				binaryWriter.Write(chars, 0, 84);
				for (int i = 0; i <= 7; i++)
				{
					text = streamReader.ReadLine();
				}
				for (int i = 0; i < 16; i++)
				{
					text = streamReader.ReadLine();
					for (int j = 0; j < 10; j++)
					{
						chars = text.PadRight(80).Substring(2 + 8 * j, 6).ToCharArray();
						binaryWriter.Write(chars, 0, 6);
					}
				}
			}
			fileStream.Seek(16288L, SeekOrigin.Begin);
			double num = -4000000.0;
			for (int k = 0; k < Sources.Count; k++)
			{
				Application.DoEvents();
				if (!Sources[k].ToBeUsed)
				{
					continue;
				}
				((Control)lblFileProcessing).set_Text(Sources[k].FileName.ToString());
				FileStream fileStream2 = new FileStream(Sources[k].File.ToString(), FileMode.Open, FileAccess.Read);
				_ = fileStream2.Length;
				StreamReader streamReader2 = new StreamReader(fileStream2);
				pBarBlock.set_Minimum(1);
				pBarBlock.set_Value(1);
				pBarBlock.set_Maximum((int)(fileStream2.Length / SourceBlockLength));
				do
				{
					string text = streamReader2.ReadLine();
					int num2 = int.Parse(text.Substring(0, 7));
					if (num2 % 10 == 0)
					{
						pBarBlock.set_Value(num2);
					}
					Application.DoEvents();
					for (int j = 1; j <= NCoeff; j += 3)
					{
						text = streamReader2.ReadLine();
						text = text.Replace("D", "E");
						array[j] = double.Parse(text.Substring(0, 26));
						try
						{
							array[j + 1] = double.Parse(text.Substring(26, 26));
						}
						catch
						{
							array[j + 1] = 0.0;
						}
						try
						{
							array[j + 2] = double.Parse(text.Substring(52, 26));
						}
						catch
						{
							array[j + 2] = 0.0;
						}
					}
					if (!(array[1] > num))
					{
						continue;
					}
					streamWriter.WriteLine(array[1]);
					num = array[1];
					for (int j = 1; j <= NCoeff; j++)
					{
						if ((j == 819) & NoNutations)
						{
							for (int l = 0; l < 80; l++)
							{
								binaryWriter.Write(0.0);
							}
						}
						binaryWriter.Write(array[j]);
					}
				}
				while (!streamReader2.EndOfStream);
				streamReader2.Close();
				listViewFiles.get_Items().get_Item(k + 1).set_ForeColor(Color.Brown);
				Application.DoEvents();
			}
			_ = fileStream.Position;
			fileStream.Close();
			streamWriter.Close();
			((Control)panelPBar).set_Visible(false);
			CheckLogFile();
		}

		private void CheckLogFile()
		{
			//IL_0062: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			FileStream fileStream = new FileStream(SourcePath + "\\" + Version + ".log", FileMode.Open, FileAccess.Read);
			StreamReader streamReader = new StreamReader(fileStream);
			int num = 0;
			double num2 = double.Parse(streamReader.ReadLine());
			do
			{
				double num3 = double.Parse(streamReader.ReadLine());
				if (num3 - num2 != 32.0)
				{
					MessageBox.Show("Log file problem at " + num3);
					num++;
				}
				num2 = num3;
			}
			while (!streamReader.EndOfStream);
			fileStream.Close();
			MessageBox.Show("DE" + Version + ".bin file created.\r\n\r\nLog file checked, with " + num + " errors", "DE" + Version + ".bin  log file check");
		}

		private void cmdSelectDEfolder_Click(object sender, EventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0015: Expected O, but got Unknown
			//IL_0037: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Invalid comparison between Unknown and I4
			//IL_0281: Unknown result type (might be due to invalid IL or missing references)
			//IL_0341: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = "";
			bool flag = false;
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_ShowNewFolderButton(false);
			val.set_SelectedPath(SourceFileFolder);
			val.set_Description("Select folder containing JPL-DE source files");
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			SourceFileFolder = val.get_SelectedPath();
			SourceHeader = "";
			((Control)lblSource).set_Text("Source file folder = " + SourceFileFolder);
			Sources.Clear();
			listViewFiles.get_Items().Clear();
			listViewFiles.get_Items().Add("File name       Start Date       End Date");
			listViewFiles.get_Items().get_Item(0).set_Checked(false);
			listViewFiles.get_Items().get_Item(0).set_ForeColor(Color.DarkBlue);
			string[] files = Directory.GetFiles(SourceFileFolder);
			foreach (string text3 in files)
			{
				if (!int.TryParse(Path.GetExtension(text3)!.Replace(".", "").PadRight(3).Substring(0, 3), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					continue;
				}
				text2 = Path.GetFileNameWithoutExtension(text3);
				if (text2.ToLower().Contains("header") && !flag)
				{
					SourceHeader = text3;
					((Control)lblHeader).set_Text("Header file = " + Path.GetFileName(text3));
					flag = true;
				}
				else if (text2.ToLower().PadRight(3).Substring(0, 3)
					.Contains("asc"))
				{
					if (text.Length == 0)
					{
						text = Path.GetExtension(text3);
						SourcePath = Path.GetDirectoryName(text3);
						Version = text.Replace(".", "");
					}
					SourceFile = new DESourceFiles();
					SourceFile.File = text3;
					SourceFile.FileName = Path.GetFileName(text3);
					using (StreamReader streamReader = new StreamReader(text3))
					{
						string text4 = streamReader.ReadLine();
						text4 = streamReader.ReadLine()!.Replace("D", "E");
						SourceFile.StartJD = double.Parse(text4.Substring(0, 26));
					}
					Sources.Add(SourceFile);
				}
			}
			Sources.Sort();
			if (SourceHeader == "")
			{
				MessageBox.Show("No header file found. Conversion cannot proceed", "No Header file", (MessageBoxButtons)0);
				return;
			}
			using (StreamReader streamReader2 = new StreamReader(SourceHeader))
			{
				string? text5 = streamReader2.ReadLine();
				int startIndex = text5!.IndexOf("NCOEFF=") + 7;
				SourceBlockLength = 26873;
				if (!int.TryParse(text5!.Substring(startIndex), out NCoeff))
				{
					NCoeff = 0;
				}
				if (NCoeff == 1018)
				{
					NoNutations = false;
					SourceBlockLength = 26873;
					if (new FileInfo(Sources[0].File).Length % SourceBlockLength != 0L)
					{
						SourceBlockLength = 26821;
					}
				}
				else
				{
					if (NCoeff != 938)
					{
						MessageBox.Show("Source files are incompatible with this conversion routine. Conversion will not proceed", "Incompatible files", (MessageBoxButtons)0);
						return;
					}
					NoNutations = true;
					SourceBlockLength = 24714;
				}
			}
			for (int j = 0; j < Sources.Count; j++)
			{
				Sources[j].EndJD = Sources[j].StartJD + (double)(new FileInfo(Sources[j].File).Length / SourceBlockLength * 32);
				listViewFiles.get_Items().Add(Sources[j].ToString());
				listViewFiles.get_Items().get_Item(j + 1).set_Checked(true);
				listViewFiles.get_Items().get_Item(j + 1).set_ForeColor(Color.Green);
				if (j > 0)
				{
					FilesAreContinuous &= Sources[j].StartJD <= Sources[j - 1].EndJD;
				}
			}
			GetCheckRange();
		}

		private void listViewFiles_Click(object sender, EventArgs e)
		{
			try
			{
				int num = listViewFiles.get_SelectedIndices().get_Item(0);
				if ((num > 0) & (num <= Sources.Count))
				{
					Sources[num - 1].ToBeUsed = !Sources[num - 1].ToBeUsed;
					for (int i = 0; i < Sources.Count; i++)
					{
						listViewFiles.get_Items().get_Item(i + 1).set_Checked(Sources[i].ToBeUsed);
						if (Sources[i].ToBeUsed)
						{
							listViewFiles.get_Items().get_Item(i + 1).set_ForeColor(Color.Green);
						}
						else
						{
							listViewFiles.get_Items().get_Item(i + 1).set_ForeColor(Color.OrangeRed);
						}
					}
				}
				GetCheckRange();
			}
			catch
			{
			}
		}

		private void GetCheckRange()
		{
			int num = 0;
			int num2 = 0;
			for (int i = 0; i < Sources.Count; i++)
			{
				if (Sources[i].ToBeUsed)
				{
					num = i;
					break;
				}
			}
			for (int num3 = Sources.Count - 1; num3 >= 0; num3--)
			{
				if (Sources[num3].ToBeUsed)
				{
					num2 = num3;
					break;
				}
			}
			HeaderStartLine = "Start Epoch: JED=" + string.Format("{0,11:f1}", Sources[num].StartJD) + " " + Utilities.Date_from_JD(Sources[num].StartJD, 0, Use_BC: false) + " 00:00:00";
			HeaderEndLine = "Final Epoch: JED=" + string.Format("{0,11:f1}", Sources[num2].EndJD) + " " + Utilities.Date_from_JD(Sources[num2].EndJD, 0, Use_BC: false) + " 00:00:00";
			FilesAreContinuous = true;
			for (int j = num + 1; j <= num2; j++)
			{
				if (j > 0)
				{
					FilesAreContinuous = FilesAreContinuous & (Sources[j].StartJD <= Sources[j - 1].EndJD) & Sources[j].ToBeUsed;
				}
			}
			if (FilesAreContinuous)
			{
				((Control)lblContinuity).set_ForeColor(Color.DarkGreen);
				((Control)lblContinuity).set_Text("Files provide continuous coverage");
			}
			else
			{
				((Control)lblContinuity).set_ForeColor(Color.DarkRed);
				((Control)lblContinuity).set_Text("Files have gaps in the range covered. Conversion cannot proceed.");
			}
			((Control)cmdCreateDEfile).set_Enabled(FilesAreContinuous);
			((Control)cmdCreateDEfile).set_Text("Create  DE" + Version + ".bin  file");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"JPL-DE Conversion");
		}

		private void cmdListVersions_Click(object sender, EventArgs e)
		{
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_002d: Invalid comparison between Unknown and I4
			//IL_0041: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				return;
			}
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				if ((int)MessageBox.Show("You must specify your Email address for anonymous FTP", "No email address", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
				{
					return;
				}
				Defaults defaults = new Defaults();
				((Control)defaults.FTPPassword).Focus();
				((Form)defaults).ShowDialog();
			}
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			lstVersions.get_Items().Clear();
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			FtpWebRequest ftpWebRequest = (FtpWebRequest)WebRequest.Create("ftp://ssd.jpl.nasa.gov/pub/eph/planets/ascii/");
			ftpWebRequest.Method = "LIST";
			ftpWebRequest.Credentials = new NetworkCredential("anonymous", fTP_AnonymousPassword);
			string[] array;
			try
			{
				FtpWebResponse ftpWebResponse = (FtpWebResponse)ftpWebRequest.GetResponse();
				StreamReader streamReader = new StreamReader(ftpWebResponse.GetResponseStream());
				array = streamReader.ReadToEnd().Split(new char[1] { '\n' });
				streamReader.Close();
				ftpWebResponse.Close();
			}
			catch
			{
				Cursor.set_Current(Cursors.get_Default());
				return;
			}
			Cursor.set_Current(Cursors.get_Default());
			for (int num = array.Length - 1; num >= 0; num--)
			{
				if (array[num].Length >= 20 && !(array[num].Substring(0, 1) != "d"))
				{
					int startIndex = array[num].LastIndexOf(" ");
					string text = array[num].Substring(startIndex).Trim();
					if (!text.EndsWith("t"))
					{
						lstVersions.get_Items().Add((object)text);
					}
				}
			}
		}

		private void lstVersions_DoubleClick(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			//IL_0045: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			bool flag2 = false;
			if (!Utilities.InternetIsAvailable())
			{
				return;
			}
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				if ((int)MessageBox.Show("You must specify your Email address for anonymous FTP", "No email address", (MessageBoxButtons)1, (MessageBoxIcon)48) == 2)
				{
					return;
				}
				Defaults defaults = new Defaults();
				((Control)defaults.FTPPassword).Focus();
				((Form)defaults).ShowDialog();
			}
			if (Settings.Default.FTP_AnonymousPassword.Length < 4)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			string fTP_AnonymousPassword = Settings.Default.FTP_AnonymousPassword;
			DownloadVersion = lstVersions.get_Items().get_Item(((ListControl)lstVersions).get_SelectedIndex()).ToString();
			((Control)lblFiles).set_Text("Files for " + DownloadVersion.ToUpper());
			FtpWebRequest ftpWebRequest = (FtpWebRequest)WebRequest.Create("ftp://ssd.jpl.nasa.gov/pub/eph/planets/ascii/" + DownloadVersion + "/");
			ftpWebRequest.Method = "LIST";
			ftpWebRequest.Credentials = new NetworkCredential("anonymous", fTP_AnonymousPassword);
			listViewDownload.get_Items().Clear();
			string[] array;
			try
			{
				FtpWebResponse ftpWebResponse = (FtpWebResponse)ftpWebRequest.GetResponse();
				StreamReader streamReader = new StreamReader(ftpWebResponse.GetResponseStream());
				array = streamReader.ReadToEnd().Split(new char[1] { '\n' });
				streamReader.Close();
				ftpWebResponse.Close();
			}
			catch
			{
				Cursor.set_Current(Cursors.get_Default());
				return;
			}
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length >= 20)
				{
					int startIndex = array[i].LastIndexOf(" ");
					string text = array[i].Substring(startIndex).Trim();
					string text2 = "";
					if (!long.TryParse(array[i].Substring(33, 10), out var result))
					{
						result = 0L;
					}
					text2 = ((result <= 500000) ? string.Format(" [{0,5:f1} kb]", (double)result / 1024.0) : string.Format(" [{0,5:f1} MB]", (double)result / 1048576.0));
					listViewDownload.get_Items().Add(text.PadRight(14) + text2);
					string text3 = Utilities.AppPath + "\\Downloaded Files\\DE_SourceFiles\\" + DownloadVersion.ToUpper() + "\\" + text;
					flag2 = false;
					flag = File.Exists(text3);
					if (flag)
					{
						flag2 = new FileInfo(text3).Length == result;
					}
					listViewDownload.get_Items().get_Item(i).set_Checked(!flag2);
					if (flag2)
					{
						listViewDownload.get_Items().get_Item(i).set_ForeColor(Color.DarkGreen);
					}
					else if (flag)
					{
						listViewDownload.get_Items().get_Item(i).set_ForeColor(Color.DarkOrange);
					}
					else
					{
						listViewDownload.get_Items().get_Item(i).set_ForeColor(Color.Black);
					}
				}
			}
		}

		private void cmdDownload_Click(object sender, EventArgs e)
		{
			string text = Utilities.AppPath + "\\Downloaded Files\\DE_SourceFiles\\" + DownloadVersion.ToUpper() + "\\";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			ftp.CancelFlag = false;
			((Control)cmdCancelDownload).set_Visible(true);
			((Control)panelProcessing).set_Visible(true);
			for (int i = 0; i < listViewDownload.get_Items().get_Count(); i++)
			{
				if (listViewDownload.get_Items().get_Item(i).get_Checked())
				{
					string fileName;
					((Control)lblFile).set_Text(fileName = listViewDownload.get_Items().get_Item(i).get_Text()
						.ToString()
						.Substring(0, 14)
						.Trim());
					if (ftp.DownloadFTP("ftp://ssd.jpl.nasa.gov/pub/eph/planets/ascii/" + DownloadVersion + "/", fileName, text, unzip: false, gunzip: false, SupressMessages: true))
					{
						listViewDownload.get_Items().get_Item(i).set_ForeColor(Color.Green);
						listViewDownload.get_Items().get_Item(i).set_Checked(false);
					}
					else
					{
						listViewDownload.get_Items().get_Item(i).set_ForeColor(Color.Red);
					}
					Application.DoEvents();
					if (ftp.CancelFlag)
					{
						break;
					}
				}
			}
			((Control)panelProcessing).set_Visible(false);
			((Control)cmdCancelDownload).set_Visible(false);
		}

		private void tabControl1_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (tabControl1.get_SelectedIndex() == 3)
			{
				GetListOfDEfiles();
			}
			if (tabControl1.get_SelectedIndex() == 2)
			{
				GetListOfDEfilesForDefaults();
			}
		}

		private void GetListOfDEfiles()
		{
			listViewDEfiles.get_Items().Clear();
			int num = 0;
			int num2 = 0;
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\", "DE*.bin");
			for (int i = 0; i < files.Length; i++)
			{
				string fileName = Path.GetFileName(files[i]);
				listViewDEfiles.get_Items().Add(fileName);
				if (fileName == JPL_DE.DE_Ephemeris_Version)
				{
					ProposedDEFile = fileName;
					listViewDEfiles.get_Items().get_Item(num).set_ForeColor(Color.DarkRed);
					listViewDEfiles.get_Items().get_Item(num).set_BackColor(Color.Yellow);
					num2 = num;
				}
				num++;
			}
			((Control)listViewDEfiles).Focus();
			if (num > 0)
			{
				listViewDEfiles.get_Items().get_Item(num2).set_Selected(true);
			}
			((Control)LBLCurrentVersion).set_Text(Utilities.EphemerisBasis());
			((Control)lblDEproposed).set_Text(JPL_DE.GetDE_VersionDetails(ProposedDEFile));
		}

		private void GetListOfDEfilesForDefaults()
		{
			listViewDEfilesForDefaults.get_Items().Clear();
			int num = 0;
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\", "DE*.bin");
			for (int i = 0; i < files.Length; i++)
			{
				string fileName = Path.GetFileName(files[i]);
				listViewDEfilesForDefaults.get_Items().Add(fileName);
				if ((fileName == "DE_Ephemeris.bin") | (fileName == "DE_LongEphemeris.bin"))
				{
					listViewDEfilesForDefaults.get_Items().get_Item(num).set_ForeColor(Color.DarkRed);
					listViewDEfilesForDefaults.get_Items().get_Item(num).set_BackColor(Color.Yellow);
				}
				num++;
			}
			((Control)listViewDEfilesForDefaults).Focus();
			if (num > 0)
			{
				listViewDEfilesForDefaults.get_Items().get_Item(0).set_Selected(true);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\DE_Ephemeris.bin"))
			{
				((Control)lblCurrentDE_Ephem).set_Text(JPL_DE.GetDE_VersionDetails("DE_Ephemeris.bin"));
			}
			else
			{
				((Control)lblCurrentDE_Ephem).set_Text("No file set");
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\DE_LongEphemeris.bin"))
			{
				((Control)lblCurrentDE_Long).set_Text(JPL_DE.GetDE_VersionDetails("DE_LongEphemeris.bin"));
			}
			else
			{
				((Control)lblCurrentDE_Long).set_Text("No file set");
			}
		}

		private void listViewDEfiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (listViewDEfiles.get_SelectedIndices().get_Count() > 0)
			{
				ProposedDEFile = listViewDEfiles.get_Items().get_Item(listViewDEfiles.get_SelectedIndices().get_Item(0)).get_Text();
			}
			((Control)lblDEproposed).set_Text(JPL_DE.GetDE_VersionDetails(ProposedDEFile));
			((Control)cmdUseSelectDEfile).set_Text("Use " + ProposedDEFile);
		}

		private void cmdUseSelectDEfile_Click(object sender, EventArgs e)
		{
			JPL_DE.DE_Ephemeris_Version = ProposedDEFile;
			JPL_DE.InitialiseDE_Ephemeris();
			JPL_DE.PurgeLunarCache();
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void listViewDEfilesForDefaults_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (listViewDEfilesForDefaults.get_SelectedIndices().get_Count() > 0)
			{
				ProposedDefaultFile = listViewDEfilesForDefaults.get_Items().get_Item(listViewDEfilesForDefaults.get_SelectedIndices().get_Item(0)).get_Text();
			}
			string dE_VersionDetails;
			((Control)lblProposedDefault).set_Text(dE_VersionDetails = JPL_DE.GetDE_VersionDetails(ProposedDefaultFile));
			string text = dE_VersionDetails;
			((Control)cmdSetDE_Ephemeris).set_Text(ProposedDefaultFile + " = " + text + "\r\n\r\nChange to DE_Ephemeris.bin file");
			((Control)cmdSetDE_Long).set_Text(ProposedDefaultFile + " = " + text + "\r\n\r\nChange to DE_LongEphemeris.bin file");
			Button obj = cmdSetDE_Ephemeris;
			bool enabled;
			((Control)cmdSetDE_Long).set_Enabled(enabled = !((ProposedDefaultFile == "DE_Ephemeris.bin") | (ProposedDefaultFile == "DE_LongEphemeris.bin")));
			((Control)obj).set_Enabled(enabled);
		}

		private void cmdSetDE_Ephemeris_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This will rename the current DE_Ephemeris file to a DE file name\r\nand rename " + ProposedDefaultFile + " to DE_Ephemeris.bin\r\n\r\nDo you want to proceed?", "Confirm change", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				JPL_DE.Set_DE_EphemerisFile(ProposedDefaultFile);
				JPL_DE.PurgeLunarCache();
				GetListOfDEfilesForDefaults();
			}
		}

		private void cmdSetDE_Long_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This will rename the current DE_LongEphemeris file to a DE file name\r\nand rename " + ProposedDefaultFile + " to DE_LongEphemeris.bin\r\n\r\nDo you want to proceed?", "Confirm change", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				JPL_DE.Set_DE_LongEphemerisFile(ProposedDefaultFile);
				JPL_DE.PurgeLunarCache();
				GetListOfDEfilesForDefaults();
			}
		}

		private void Create_JPL_DE_EphemerisFile_FormClosed(object sender, FormClosedEventArgs e)
		{
			JPL_DE.CreateDEfile = null;
		}

		private void cmdClose_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void Create_JPL_DE_EphemerisFile_FormClosing(object sender, FormClosingEventArgs e)
		{
			Utilities.Set_JPLDE_Availability();
		}

		private void cmdCancelDownload_Click(object sender, EventArgs e)
		{
			ftp.CancelFlag = true;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_0a96: Unknown result type (might be due to invalid IL or missing references)
			//IL_1220: Unknown result type (might be due to invalid IL or missing references)
			//IL_213f: Unknown result type (might be due to invalid IL or missing references)
			//IL_2149: Expected O, but got Unknown
			//IL_2151: Unknown result type (might be due to invalid IL or missing references)
			//IL_215b: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Create_JPL_DE_EphemerisFile));
			cmdSelectDEfolder = new Button();
			lblSource = new Label();
			lblHeader = new Label();
			cmdCreateDEfile = new Button();
			pBarBlock = new ProgressBar();
			panelPBar = new Panel();
			lblFileProcessing = new Label();
			label4 = new Label();
			label2 = new Label();
			lblContinuity = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			tabControl1 = new TabControl();
			tabDownload = new TabPage();
			cmdCancelDownload = new Button();
			label8 = new Label();
			label7 = new Label();
			label3 = new Label();
			label1 = new Label();
			listViewDownload = new ListView();
			cmdDownload = new Button();
			lblFiles = new Label();
			lstVersions = new ListBox();
			cmdListVersions = new Button();
			panelProcessing = new Panel();
			label6 = new Label();
			lblFile = new Label();
			tabConvert = new TabPage();
			listViewFiles = new ListView();
			tabDefaults = new TabPage();
			lblProposedDefault = new Label();
			grpSetDE_Long = new GroupBox();
			cmdSetDE_Long = new Button();
			label14 = new Label();
			lblCurrentDE_Long = new Label();
			grpSetDEEphemeris = new GroupBox();
			cmdSetDE_Ephemeris = new Button();
			lblCurrentDE_Ephem = new Label();
			label13 = new Label();
			label12 = new Label();
			listViewDEfilesForDefaults = new ListView();
			tabSetTemporary = new TabPage();
			label15 = new Label();
			cmdClose = new Button();
			label11 = new Label();
			cmdUseSelectDEfile = new Button();
			lblLabel11 = new Label();
			lblDEproposed = new Label();
			label10 = new Label();
			LBLCurrentVersion = new Label();
			listViewDEfiles = new ListView();
			label9 = new Label();
			label5 = new Label();
			toolTip1 = new ToolTip(components);
			((Control)panelPBar).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)tabControl1).SuspendLayout();
			((Control)tabDownload).SuspendLayout();
			((Control)panelProcessing).SuspendLayout();
			((Control)tabConvert).SuspendLayout();
			((Control)tabDefaults).SuspendLayout();
			((Control)grpSetDE_Long).SuspendLayout();
			((Control)grpSetDEEphemeris).SuspendLayout();
			((Control)tabSetTemporary).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdSelectDEfolder).set_Location(new Point(112, 17));
			((Control)cmdSelectDEfolder).set_Name("cmdSelectDEfolder");
			((Control)cmdSelectDEfolder).set_Size(new Size(265, 27));
			((Control)cmdSelectDEfolder).set_TabIndex(1);
			((Control)cmdSelectDEfolder).set_Text("Select the folder containing JPL-DE source files");
			((ButtonBase)cmdSelectDEfolder).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectDEfolder).add_Click((EventHandler)cmdSelectDEfolder_Click);
			((Control)lblSource).set_AutoSize(true);
			((Control)lblSource).set_Location(new Point(6, 49));
			((Control)lblSource).set_Name("lblSource");
			((Control)lblSource).set_Size(new Size(98, 13));
			((Control)lblSource).set_TabIndex(2);
			((Control)lblSource).set_Text("Source file folder = ");
			((Control)lblHeader).set_AutoSize(true);
			((Control)lblHeader).set_Location(new Point(6, 67));
			((Control)lblHeader).set_Name("lblHeader");
			((Control)lblHeader).set_Size(new Size(70, 13));
			((Control)lblHeader).set_TabIndex(3);
			((Control)lblHeader).set_Text("Header file = ");
			((Control)cmdCreateDEfile).set_Enabled(false);
			((Control)cmdCreateDEfile).set_Location(new Point(73, 355));
			((Control)cmdCreateDEfile).set_Name("cmdCreateDEfile");
			((Control)cmdCreateDEfile).set_Size(new Size(138, 41));
			((Control)cmdCreateDEfile).set_TabIndex(5);
			((Control)cmdCreateDEfile).set_Text("Create DExxx file");
			((ButtonBase)cmdCreateDEfile).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateDEfile).add_Click((EventHandler)cmdCreateDEfile_Click);
			((Control)pBarBlock).set_Location(new Point(40, 36));
			pBarBlock.set_MarqueeAnimationSpeed(1);
			pBarBlock.set_Maximum(10);
			((Control)pBarBlock).set_Name("pBarBlock");
			((Control)pBarBlock).set_Size(new Size(130, 9));
			pBarBlock.set_Step(1);
			((Control)pBarBlock).set_TabIndex(7);
			((Control)panelPBar).get_Controls().Add((Control)(object)lblFileProcessing);
			((Control)panelPBar).get_Controls().Add((Control)(object)label4);
			((Control)panelPBar).get_Controls().Add((Control)(object)label2);
			((Control)panelPBar).get_Controls().Add((Control)(object)pBarBlock);
			((Control)panelPBar).set_Location(new Point(236, 350));
			((Control)panelPBar).set_Name("panelPBar");
			((Control)panelPBar).set_Size(new Size(179, 49));
			((Control)panelPBar).set_TabIndex(8);
			((Control)panelPBar).set_Visible(false);
			((Control)lblFileProcessing).set_AutoSize(true);
			((Control)lblFileProcessing).set_Location(new Point(39, 17));
			((Control)lblFileProcessing).set_Name("lblFileProcessing");
			((Control)lblFileProcessing).set_Size(new Size(27, 13));
			((Control)lblFileProcessing).set_TabIndex(11);
			((Control)lblFileProcessing).set_Text("xxxx");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(55, 3));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(68, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("Processing...");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(3, 17));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(23, 13));
			((Control)label2).set_TabIndex(8);
			((Control)label2).set_Text("File");
			((Control)lblContinuity).set_AutoSize(true);
			((Control)lblContinuity).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblContinuity).set_ForeColor(Color.DarkRed);
			((Control)lblContinuity).set_Location(new Point(3, 331));
			((Control)lblContinuity).set_Name("lblContinuity");
			((Control)lblContinuity).set_Size(new Size(63, 13));
			((Control)lblContinuity).set_TabIndex(10);
			((Control)lblContinuity).set_Text("Continuity");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(515, 24));
			((Control)menuStrip1).set_TabIndex(11);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)tabControl1).get_Controls().Add((Control)(object)tabDownload);
			((Control)tabControl1).get_Controls().Add((Control)(object)tabConvert);
			((Control)tabControl1).get_Controls().Add((Control)(object)tabDefaults);
			((Control)tabControl1).get_Controls().Add((Control)(object)tabSetTemporary);
			((Control)tabControl1).set_Location(new Point(12, 48));
			((Control)tabControl1).set_Name("tabControl1");
			tabControl1.set_SelectedIndex(0);
			((Control)tabControl1).set_Size(new Size(496, 428));
			((Control)tabControl1).set_TabIndex(12);
			tabControl1.add_SelectedIndexChanged((EventHandler)tabControl1_SelectedIndexChanged);
			((Control)tabDownload).get_Controls().Add((Control)(object)cmdCancelDownload);
			((Control)tabDownload).get_Controls().Add((Control)(object)label8);
			((Control)tabDownload).get_Controls().Add((Control)(object)label7);
			((Control)tabDownload).get_Controls().Add((Control)(object)label3);
			((Control)tabDownload).get_Controls().Add((Control)(object)label1);
			((Control)tabDownload).get_Controls().Add((Control)(object)listViewDownload);
			((Control)tabDownload).get_Controls().Add((Control)(object)cmdDownload);
			((Control)tabDownload).get_Controls().Add((Control)(object)lblFiles);
			((Control)tabDownload).get_Controls().Add((Control)(object)lstVersions);
			((Control)tabDownload).get_Controls().Add((Control)(object)cmdListVersions);
			((Control)tabDownload).get_Controls().Add((Control)(object)panelProcessing);
			tabDownload.set_Location(new Point(4, 22));
			((Control)tabDownload).set_Name("tabDownload");
			((Control)tabDownload).set_Padding(new Padding(3));
			((Control)tabDownload).set_Size(new Size(488, 402));
			tabDownload.set_TabIndex(0);
			((Control)tabDownload).set_Text("Download DE source files");
			tabDownload.set_UseVisualStyleBackColor(true);
			((Control)cmdCancelDownload).set_BackColor(Color.Yellow);
			((Control)cmdCancelDownload).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancelDownload).set_ForeColor(Color.Red);
			((Control)cmdCancelDownload).set_Location(new Point(329, 64));
			((Control)cmdCancelDownload).set_Name("cmdCancelDownload");
			((Control)cmdCancelDownload).set_Size(new Size(143, 31));
			((Control)cmdCancelDownload).set_TabIndex(20);
			((Control)cmdCancelDownload).set_Text("Cancel download");
			((ButtonBase)cmdCancelDownload).set_UseVisualStyleBackColor(false);
			((Control)cmdCancelDownload).set_Visible(false);
			((Control)cmdCancelDownload).add_Click((EventHandler)cmdCancelDownload_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_ForeColor(Color.Red);
			((Control)label8).set_Location(new Point(222, 50));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(103, 13));
			((Control)label8).set_TabIndex(19);
			((Control)label8).set_Text("Download has failed");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_ForeColor(Color.DarkGreen);
			((Control)label7).set_Location(new Point(76, 50));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(131, 13));
			((Control)label7).set_TabIndex(18);
			((Control)label7).set_Text("File has been downloaded");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_ForeColor(Color.DarkOrange);
			((Control)label3).set_Location(new Point(76, 37));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(254, 13));
			((Control)label3).set_TabIndex(17);
			((Control)label3).set_Text("File has been downloaded, but has the wrong length");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(76, 24));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(149, 13));
			((Control)label1).set_TabIndex(16);
			((Control)label1).set_Text("File has not been downloaded");
			listViewDownload.set_CheckBoxes(true);
			((Control)listViewDownload).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			listViewDownload.set_HideSelection(false);
			((Control)listViewDownload).set_Location(new Point(77, 69));
			((Control)listViewDownload).set_Name("listViewDownload");
			((Control)listViewDownload).set_Size(new Size(395, 326));
			((Control)listViewDownload).set_TabIndex(15);
			listViewDownload.set_UseCompatibleStateImageBehavior(false);
			listViewDownload.set_View((View)3);
			((Control)cmdDownload).set_Location(new Point(329, 6));
			((Control)cmdDownload).set_Name("cmdDownload");
			((Control)cmdDownload).set_Size(new Size(143, 31));
			((Control)cmdDownload).set_TabIndex(5);
			((Control)cmdDownload).set_Text("Download selected files");
			((ButtonBase)cmdDownload).set_UseVisualStyleBackColor(true);
			((Control)cmdDownload).add_Click((EventHandler)cmdDownload_Click);
			((Control)lblFiles).set_AutoSize(true);
			((Control)lblFiles).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFiles).set_Location(new Point(140, 7));
			((Control)lblFiles).set_Name("lblFiles");
			((Control)lblFiles).set_Size(new Size(113, 17));
			((Control)lblFiles).set_TabIndex(4);
			((Control)lblFiles).set_Text("Files for DE....");
			((Control)lstVersions).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ListControl)lstVersions).set_FormattingEnabled(true);
			lstVersions.set_ItemHeight(14);
			((Control)lstVersions).set_Location(new Point(7, 69));
			((Control)lstVersions).set_Name("lstVersions");
			((Control)lstVersions).set_Size(new Size(64, 326));
			((Control)lstVersions).set_TabIndex(1);
			toolTip1.SetToolTip((Control)(object)lstVersions, "double-click to list files");
			((Control)lstVersions).add_DoubleClick((EventHandler)lstVersions_DoubleClick);
			((Control)cmdListVersions).set_Location(new Point(7, 6));
			((Control)cmdListVersions).set_Name("cmdListVersions");
			((Control)cmdListVersions).set_Size(new Size(64, 52));
			((Control)cmdListVersions).set_TabIndex(0);
			((Control)cmdListVersions).set_Text("Get available versions");
			((ButtonBase)cmdListVersions).set_UseVisualStyleBackColor(true);
			((Control)cmdListVersions).add_Click((EventHandler)cmdListVersions_Click);
			((Control)panelProcessing).get_Controls().Add((Control)(object)label6);
			((Control)panelProcessing).get_Controls().Add((Control)(object)lblFile);
			((Control)panelProcessing).set_Location(new Point(338, 35));
			((Control)panelProcessing).set_Name("panelProcessing");
			((Control)panelProcessing).set_Size(new Size(129, 28));
			((Control)panelProcessing).set_TabIndex(14);
			((Control)panelProcessing).set_Visible(false);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(6, 1));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(78, 13));
			((Control)label6).set_TabIndex(13);
			((Control)label6).set_Text("Downloading...");
			((Control)lblFile).set_AutoSize(true);
			((Control)lblFile).set_Location(new Point(6, 13));
			((Control)lblFile).set_Name("lblFile");
			((Control)lblFile).set_Size(new Size(23, 13));
			((Control)lblFile).set_TabIndex(12);
			((Control)lblFile).set_Text("File");
			((Control)tabConvert).get_Controls().Add((Control)(object)lblSource);
			((Control)tabConvert).get_Controls().Add((Control)(object)listViewFiles);
			((Control)tabConvert).get_Controls().Add((Control)(object)cmdSelectDEfolder);
			((Control)tabConvert).get_Controls().Add((Control)(object)lblHeader);
			((Control)tabConvert).get_Controls().Add((Control)(object)cmdCreateDEfile);
			((Control)tabConvert).get_Controls().Add((Control)(object)lblContinuity);
			((Control)tabConvert).get_Controls().Add((Control)(object)panelPBar);
			tabConvert.set_Location(new Point(4, 22));
			((Control)tabConvert).set_Name("tabConvert");
			((Control)tabConvert).set_Padding(new Padding(3));
			((Control)tabConvert).set_Size(new Size(488, 402));
			tabConvert.set_TabIndex(1);
			((Control)tabConvert).set_Text("Convert DE source files");
			tabConvert.set_UseVisualStyleBackColor(true);
			listViewFiles.set_CheckBoxes(true);
			((Control)listViewFiles).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			listViewFiles.set_HideSelection(false);
			((Control)listViewFiles).set_Location(new Point(8, 85));
			listViewFiles.set_MultiSelect(false);
			((Control)listViewFiles).set_Name("listViewFiles");
			((Control)listViewFiles).set_Size(new Size(469, 236));
			((Control)listViewFiles).set_TabIndex(11);
			listViewFiles.set_UseCompatibleStateImageBehavior(false);
			listViewFiles.set_View((View)3);
			((Control)listViewFiles).add_Click((EventHandler)listViewFiles_Click);
			((Control)tabDefaults).get_Controls().Add((Control)(object)lblProposedDefault);
			((Control)tabDefaults).get_Controls().Add((Control)(object)grpSetDE_Long);
			((Control)tabDefaults).get_Controls().Add((Control)(object)grpSetDEEphemeris);
			((Control)tabDefaults).get_Controls().Add((Control)(object)label12);
			((Control)tabDefaults).get_Controls().Add((Control)(object)listViewDEfilesForDefaults);
			tabDefaults.set_Location(new Point(4, 22));
			((Control)tabDefaults).set_Name("tabDefaults");
			((Control)tabDefaults).set_Size(new Size(488, 402));
			tabDefaults.set_TabIndex(2);
			((Control)tabDefaults).set_Text("Set default versions");
			tabDefaults.set_UseVisualStyleBackColor(true);
			((Control)lblProposedDefault).set_AutoSize(true);
			((Control)lblProposedDefault).set_Location(new Point(59, 39));
			((Control)lblProposedDefault).set_Name("lblProposedDefault");
			((Control)lblProposedDefault).set_Size(new Size(90, 13));
			((Control)lblProposedDefault).set_TabIndex(12);
			((Control)lblProposedDefault).set_Text(" Proposed default");
			((Control)grpSetDE_Long).get_Controls().Add((Control)(object)cmdSetDE_Long);
			((Control)grpSetDE_Long).get_Controls().Add((Control)(object)label14);
			((Control)grpSetDE_Long).get_Controls().Add((Control)(object)lblCurrentDE_Long);
			((Control)grpSetDE_Long).set_Location(new Point(181, 253));
			((Control)grpSetDE_Long).set_Name("grpSetDE_Long");
			((Control)grpSetDE_Long).set_Size(new Size(268, 142));
			((Control)grpSetDE_Long).set_TabIndex(11);
			grpSetDE_Long.set_TabStop(false);
			((Control)grpSetDE_Long).set_Text("Set DE Long Ephemeris");
			((Control)cmdSetDE_Long).set_Location(new Point(12, 69));
			((Control)cmdSetDE_Long).set_Name("cmdSetDE_Long");
			((Control)cmdSetDE_Long).set_Size(new Size(244, 65));
			((Control)cmdSetDE_Long).set_TabIndex(4);
			((Control)cmdSetDE_Long).set_Text("Set");
			((ButtonBase)cmdSetDE_Long).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDE_Long).add_Click((EventHandler)cmdSetDE_Long_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(10, 21));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(183, 13));
			((Control)label14).set_TabIndex(1);
			((Control)label14).set_Text("Current DE_LongEphemeris.bin");
			((Control)lblCurrentDE_Long).set_AutoSize(true);
			((Control)lblCurrentDE_Long).set_Location(new Point(10, 34));
			((Control)lblCurrentDE_Long).set_Name("lblCurrentDE_Long");
			((Control)lblCurrentDE_Long).set_Size(new Size(52, 13));
			((Control)lblCurrentDE_Long).set_TabIndex(1);
			((Control)lblCurrentDE_Long).set_Text("DE_Long");
			((Control)grpSetDEEphemeris).get_Controls().Add((Control)(object)cmdSetDE_Ephemeris);
			((Control)grpSetDEEphemeris).get_Controls().Add((Control)(object)lblCurrentDE_Ephem);
			((Control)grpSetDEEphemeris).get_Controls().Add((Control)(object)label13);
			((Control)grpSetDEEphemeris).set_Location(new Point(181, 101));
			((Control)grpSetDEEphemeris).set_Name("grpSetDEEphemeris");
			((Control)grpSetDEEphemeris).set_Size(new Size(268, 142));
			((Control)grpSetDEEphemeris).set_TabIndex(10);
			grpSetDEEphemeris.set_TabStop(false);
			((Control)grpSetDEEphemeris).set_Text("Set DE Ephemeris");
			((Control)cmdSetDE_Ephemeris).set_Location(new Point(12, 69));
			((Control)cmdSetDE_Ephemeris).set_Name("cmdSetDE_Ephemeris");
			((Control)cmdSetDE_Ephemeris).set_Size(new Size(244, 65));
			((Control)cmdSetDE_Ephemeris).set_TabIndex(3);
			((Control)cmdSetDE_Ephemeris).set_Text("Set");
			((ButtonBase)cmdSetDE_Ephemeris).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDE_Ephemeris).add_Click((EventHandler)cmdSetDE_Ephemeris_Click);
			((Control)lblCurrentDE_Ephem).set_AutoSize(true);
			((Control)lblCurrentDE_Ephem).set_Location(new Point(10, 34));
			((Control)lblCurrentDE_Ephem).set_Name("lblCurrentDE_Ephem");
			((Control)lblCurrentDE_Ephem).set_Size(new Size(61, 13));
			((Control)lblCurrentDE_Ephem).set_TabIndex(2);
			((Control)lblCurrentDE_Ephem).set_Text("DE_Ephem");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(10, 21));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(155, 13));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("Current DE_Ephemeris.bin");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(5, 83));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(155, 13));
			((Control)label12).set_TabIndex(9);
			((Control)label12).set_Text("Available DE ephemerides");
			((Control)listViewDEfilesForDefaults).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			listViewDEfilesForDefaults.set_HideSelection(false);
			((Control)listViewDEfilesForDefaults).set_Location(new Point(3, 99));
			listViewDEfilesForDefaults.set_MultiSelect(false);
			((Control)listViewDEfilesForDefaults).set_Name("listViewDEfilesForDefaults");
			((Control)listViewDEfilesForDefaults).set_Size(new Size(157, 300));
			((Control)listViewDEfilesForDefaults).set_TabIndex(8);
			listViewDEfilesForDefaults.set_UseCompatibleStateImageBehavior(false);
			listViewDEfilesForDefaults.set_View((View)3);
			listViewDEfilesForDefaults.add_SelectedIndexChanged((EventHandler)listViewDEfilesForDefaults_SelectedIndexChanged);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)label15);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)cmdClose);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)label11);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)cmdUseSelectDEfile);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)lblLabel11);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)lblDEproposed);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)label10);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)LBLCurrentVersion);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)listViewDEfiles);
			((Control)tabSetTemporary).get_Controls().Add((Control)(object)label9);
			tabSetTemporary.set_Location(new Point(4, 22));
			((Control)tabSetTemporary).set_Name("tabSetTemporary");
			((Control)tabSetTemporary).set_Size(new Size(488, 402));
			tabSetTemporary.set_TabIndex(3);
			((Control)tabSetTemporary).set_Text("Set temporary version");
			tabSetTemporary.set_UseVisualStyleBackColor(true);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(15, 10));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(454, 13));
			((Control)label15).set_TabIndex(9);
			((Control)label15).set_Text("Temporarily set the DE Ephemeris being used in the current session of Occult. ");
			((Control)cmdClose).set_Location(new Point(238, 319));
			((Control)cmdClose).set_Name("cmdClose");
			((Control)cmdClose).set_Size(new Size(137, 36));
			((Control)cmdClose).set_TabIndex(8);
			((Control)cmdClose).set_Text("Close - with no changes");
			((ButtonBase)cmdClose).set_UseVisualStyleBackColor(true);
			((Control)cmdClose).add_Click((EventHandler)cmdClose_Click);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(21, 115));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(155, 13));
			((Control)label11).set_TabIndex(7);
			((Control)label11).set_Text("Available DE ephemerides");
			((Control)cmdUseSelectDEfile).set_Location(new Point(238, 260));
			((Control)cmdUseSelectDEfile).set_Name("cmdUseSelectDEfile");
			((Control)cmdUseSelectDEfile).set_Size(new Size(137, 42));
			((Control)cmdUseSelectDEfile).set_TabIndex(6);
			((Control)cmdUseSelectDEfile).set_Text("Set selected DE file\r\n&& close");
			((ButtonBase)cmdUseSelectDEfile).set_UseVisualStyleBackColor(true);
			((Control)cmdUseSelectDEfile).add_Click((EventHandler)cmdUseSelectDEfile_Click);
			((Control)lblLabel11).set_AutoSize(true);
			((Control)lblLabel11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblLabel11).set_Location(new Point(238, 186));
			((Control)lblLabel11).set_Name("lblLabel11");
			((Control)lblLabel11).set_Size(new Size(170, 13));
			((Control)lblLabel11).set_TabIndex(5);
			((Control)lblLabel11).set_Text("Coverage of selected DE file");
			((Control)lblDEproposed).set_AutoSize(true);
			((Control)lblDEproposed).set_Location(new Point(238, 208));
			((Control)lblDEproposed).set_Name("lblDEproposed");
			((Control)lblDEproposed).set_Size(new Size(52, 13));
			((Control)lblDEproposed).set_TabIndex(4);
			((Control)lblDEproposed).set_Text("Proposed");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(238, 122));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(211, 13));
			((Control)label10).set_TabIndex(3);
			((Control)label10).set_Text("Coverage of current default DE files");
			((Control)LBLCurrentVersion).set_AutoSize(true);
			((Control)LBLCurrentVersion).set_Location(new Point(238, 144));
			((Control)LBLCurrentVersion).set_Name("LBLCurrentVersion");
			((Control)LBLCurrentVersion).set_Size(new Size(41, 13));
			((Control)LBLCurrentVersion).set_TabIndex(2);
			((Control)LBLCurrentVersion).set_Text("Current");
			((Control)listViewDEfiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			listViewDEfiles.set_HideSelection(false);
			((Control)listViewDEfiles).set_Location(new Point(19, 131));
			listViewDEfiles.set_MultiSelect(false);
			((Control)listViewDEfiles).set_Name("listViewDEfiles");
			((Control)listViewDEfiles).set_Size(new Size(157, 254));
			((Control)listViewDEfiles).set_TabIndex(1);
			listViewDEfiles.set_UseCompatibleStateImageBehavior(false);
			listViewDEfiles.set_View((View)3);
			listViewDEfiles.add_SelectedIndexChanged((EventHandler)listViewDEfiles_SelectedIndexChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(14, 32));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(367, 52));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text(componentResourceManager.GetString("label9.Text"));
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.Yellow);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkRed);
			((Control)label5).set_Location(new Point(103, 29));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(308, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("This routine cannot convert DE 102, 200, 202, && 406");
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(5000);
			toolTip1.set_InitialDelay(10);
			toolTip1.set_ReshowDelay(20);
			toolTip1.set_ShowAlways(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(515, 489));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)tabControl1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("Create_JPL_DE_EphemerisFile");
			((Control)this).set_Text("Create JPL_DE ephemeris file");
			((Form)this).add_FormClosing(new FormClosingEventHandler(Create_JPL_DE_EphemerisFile_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(Create_JPL_DE_EphemerisFile_FormClosed));
			((Form)this).add_Load((EventHandler)Create_JPL_DE_EphemerisFile_Load);
			((Control)panelPBar).ResumeLayout(false);
			((Control)panelPBar).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)tabControl1).ResumeLayout(false);
			((Control)tabDownload).ResumeLayout(false);
			((Control)tabDownload).PerformLayout();
			((Control)panelProcessing).ResumeLayout(false);
			((Control)panelProcessing).PerformLayout();
			((Control)tabConvert).ResumeLayout(false);
			((Control)tabConvert).PerformLayout();
			((Control)tabDefaults).ResumeLayout(false);
			((Control)tabDefaults).PerformLayout();
			((Control)grpSetDE_Long).ResumeLayout(false);
			((Control)grpSetDE_Long).PerformLayout();
			((Control)grpSetDEEphemeris).ResumeLayout(false);
			((Control)grpSetDEEphemeris).PerformLayout();
			((Control)tabSetTemporary).ResumeLayout(false);
			((Control)tabSetTemporary).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
