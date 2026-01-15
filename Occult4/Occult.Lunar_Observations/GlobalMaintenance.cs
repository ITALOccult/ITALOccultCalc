using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class GlobalMaintenance : Form
	{
		private string GrazeArchiveFile = "";

		private const int RecordLength = 270;

		private IContainer components;

		private Panel panelCombine;

		private GroupBox groupBox9;

		private CheckBox chkVizierLimitTo1960;

		private Button cmdVizier;

		private Label label63;

		private Label label56;

		private TextBox txtEmailSignatureName;

		private Label label40;

		private CheckBox checkBox1;

		private CheckBox checkBox2;

		private Button cmdGenerateGrazeIndex;

		private Label label36;

		private ComboBox cmbObservationsSource;

		private Button cmdProcessfromCoordinators;

		private Button cmdGZip;

		private NumericUpDown updnYear;

		private Button cmdSort;

		private NumericUpDown updnGrazeRenumberYear;

		private Button cmdReduceAll;

		private Label label3;

		private Label label2;

		private Label label1;

		public GlobalMaintenance()
		{
			InitializeComponent();
		}

		private void GlobalMaintenance_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((ListControl)cmbObservationsSource).set_SelectedIndex(0);
			updnGrazeRenumberYear.set_Value((decimal)DateTime.Now.Year - 5m);
		}

		private void cmdProcessfromCoordinators_Click(object sender, EventArgs e)
		{
			//IL_0010: Unknown result type (might be due to invalid IL or missing references)
			//IL_0017: Expected O, but got Unknown
			//IL_0078: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Invalid comparison between Unknown and I4
			//IL_0314: Unknown result type (might be due to invalid IL or missing references)
			//IL_031a: Invalid comparison between Unknown and I4
			//IL_0450: Unknown result type (might be due to invalid IL or missing references)
			//IL_0542: Unknown result type (might be due to invalid IL or missing references)
			//IL_059e: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Invalid comparison between Unknown and I4
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_06aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b0: Invalid comparison between Unknown and I4
			//IL_06e9: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			Cursor.set_Current(Cursors.get_WaitCursor());
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive files to Process. ['MergedFiles', 'Archive Observations', 'Log File', 'Manually Added' + .zip .rar .arj .lzh .gz & .tar files will be ignored]");
			if (Settings.Default.LunarArchiveLastConsolidation == "")
			{
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Lunar Archive");
			}
			else
			{
				((FileDialog)val).set_InitialDirectory(Settings.Default.LunarArchiveLastConsolidation);
			}
			((FileDialog)val).set_FileName("*.*");
			val.set_Multiselect(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				int upperBound = ((FileDialog)val).get_FileNames().GetUpperBound(0);
				if (upperBound < 0)
				{
					return;
				}
				Settings.Default.LunarArchiveLastConsolidation = Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0]);
				string text2 = Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0]) + "\\MergedFiles.txt";
				if (File.Exists(text2))
				{
					File.Delete(text2);
				}
				text = Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0]) + "\\Log File.txt";
				if (File.Exists(text))
				{
					File.Delete(text);
				}
				using (StreamWriter streamWriter = new StreamWriter(text))
				{
					streamWriter.WriteLine("Archive Observations recent.dat created\r\non " + DateTime.Now.ToUniversalTime().ToLongDateString() + " UT, using:");
					streamWriter.WriteLine("");
					for (int i = 0; i <= upperBound; i++)
					{
						if (((FileDialog)val).get_FileNames()[i].Contains("MergedFiles") || ((FileDialog)val).get_FileNames()[i].Contains("Archive Observations") || ((FileDialog)val).get_FileNames()[i].Contains("Log File") || ((FileDialog)val).get_FileNames()[i].ToLower().Contains("manually added") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".zip") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".rar") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".arj") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".lzh") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".gz") || ((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".tar"))
						{
							continue;
						}
						if (new FileInfo(((FileDialog)val).get_FileNames()[i]).Length % 270 != 0L)
						{
							string text3 = "Lines ";
							int num = 0;
							using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileNames()[i], Encoding.ASCII))
							{
								do
								{
									num++;
									if (streamReader.ReadLine()!.Length != 268)
									{
										text3 = text3 + num + ", ";
									}
								}
								while (!streamReader.EndOfStream);
							}
							if ((int)MessageBox.Show(((FileDialog)val).get_FileNames()[i] + "\r\nis of incorrect length and will not be added. Errors at:\r\n" + text3 + "\r\n\r\nContinue?", "Incorrect file length", (MessageBoxButtons)1, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
							{
								return;
							}
						}
						else
						{
							File.AppendAllText(text2, File.ReadAllText(((FileDialog)val).get_FileNames()[i]));
						}
						streamWriter.WriteLine(Path.GetFileName(((FileDialog)val).get_FileNames()[i]));
					}
					streamWriter.WriteLine("* * * * *");
				}
				LunarObservations.AutoCorrectArchiveFile(text2, "AHIMNOPRSX".Substring(((ListControl)cmbObservationsSource).get_SelectedIndex(), 1));
				SortFile(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt");
				if (new FileInfo(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt").Length % 270 != 0L)
				{
					string text4 = "Lines ";
					int num2 = 0;
					using (StreamReader streamReader2 = new StreamReader(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt", Encoding.ASCII))
					{
						do
						{
							num2++;
							if (streamReader2.ReadLine()!.Length != 268)
							{
								text4 = text4 + num2 + ", ";
							}
						}
						while (!streamReader2.EndOfStream);
					}
					MessageBox.Show("The Sorted & Corrected file is of incorrect length. Errors at:\r\n" + text4 + "\r\n\r\nMost likely a source file contains accented characters that will need \r\nto be manually edited out before the process can run to completion.", "Incorrect file length", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				ListDuplicates(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt");
				ArrayList arrayList = new ArrayList();
				try
				{
					if (LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Count() > 0)
					{
						try
						{
							for (int j = 0; j < LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Count(); j++)
							{
								arrayList.Add(LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Item(j).ToString());
							}
							((Form)LunarObservations.ArchiveDuplicates).Close();
						}
						catch
						{
						}
						LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
						((Form)LunarObservations.ArchiveDuplicates).set_DialogResult((DialogResult)2);
						try
						{
							for (int k = 0; k < arrayList.Count; k++)
							{
								LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)arrayList[k]!.ToString());
							}
							LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
							((Form)LunarObservations.ArchiveDuplicates).ShowDialog();
						}
						catch
						{
							LunarObservations.ArchiveDuplicates = new ListDuplicates();
							for (int l = 0; l < arrayList.Count; l++)
							{
								LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)arrayList[l]!.ToString());
							}
							LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
							((Form)LunarObservations.ArchiveDuplicates).ShowDialog();
						}
						if ((int)((Form)LunarObservations.ArchiveDuplicates).get_DialogResult() == 2)
						{
							return;
						}
					}
				}
				catch
				{
					MessageBox.Show("Error with Duplicates.\r\n\r\n Processing cancelled", "Error", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				string text5 = Path.GetDirectoryName(text2) + "\\Archive Observations recent.dat";
				if (File.Exists(Path.GetDirectoryName(text2) + "\\Archive Observations recent.dat"))
				{
					File.Delete(text5);
				}
				File.Move(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt", text5);
				GrazeArchiveFile = text5;
				RenumberGrazes();
				if (File.Exists(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt"))
				{
					File.Delete(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt");
				}
				if (File.Exists(Path.GetDirectoryName(text2) + "\\MergedFiles.txt"))
				{
					File.Delete(Path.GetDirectoryName(text2) + "\\MergedFiles.txt");
				}
				LunarObservations.ReduceArchiveOccultationFile(text5, UsingSiteID: false, Settings.Default.ArchiveGrazesExcludeStartEnd, Settings.Default.ArchiveGrazesExcludeInvalid);
			}
			if ((int)MessageBox.Show("Do you want to email the Log file to the regional coordinators?", "Confirm send Email", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				string subject = "Recent lunar occultation observations updated - " + DateTime.Now.ToShortDateString();
				MessageBox.Show(Emails.Email_LogFile(text, ((Control)txtEmailSignatureName).get_Text().Trim(), subject), "Success");
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGenerateGrazeIndex_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateGrazeIndexFile();
		}

		private void cmdVizier_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateVizierArchiveFile();
			GZipArchive();
		}

		private void RenumberGrazes()
		{
			if (GrazeArchiveFile.Length < 2)
			{
				return;
			}
			FileStream fileStream = new FileStream(GrazeArchiveFile, FileMode.Open, FileAccess.ReadWrite);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			string text = "";
			string text2 = "       ";
			string text3 = "";
			string text4 = "";
			string[] array = new string[6] { "", "", "", "", "", "" };
			string[] array2 = new string[6] { "", "", "", "", "", "" };
			int num = 0;
			int num2 = (int)updnGrazeRenumberYear.get_Value();
			for (int i = 0; i < fileStream.Length / 270; i++)
			{
				fileStream.Seek(i * 270 + 28, SeekOrigin.Begin);
				string text5 = new string(binaryReader.ReadChars(1));
				fileStream.Seek(i * 270, SeekOrigin.Begin);
				string text6 = new string(binaryReader.ReadChars(4));
				if (int.Parse(text6) < num2)
				{
					continue;
				}
				fileStream.Seek(i * 270 + 6, SeekOrigin.Begin);
				text = new string(binaryReader.ReadChars(2));
				if (text != text4)
				{
					text4 = text;
					for (int num3 = 5; num3 > 0; num3--)
					{
						array[num3] = array[num3 - 1];
						array2[num3] = array2[num3 - 1];
					}
					array[0] = "-1000";
				}
				if (text6 != text3)
				{
					text3 = text6;
					num = 0;
				}
				if (text5 == "G")
				{
					fileStream.Seek(i * 270 + 18, SeekOrigin.Begin);
					string text7 = new string(binaryReader.ReadChars(7));
					fileStream.Seek(i * 270 + 70, SeekOrigin.Begin);
					string text8 = new string(binaryReader.ReadChars(1));
					text7 = text7 + " " + text8;
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					new string(binaryReader.ReadChars(7));
					bool flag = false;
					for (int j = 0; j <= 5; j++)
					{
						if (text7 == array[j])
						{
							text2 = array2[j];
							flag = true;
							break;
						}
					}
					if (!flag)
					{
						num++;
						for (int num4 = 5; num4 > 0; num4--)
						{
							array[num4] = array[num4 - 1];
							array2[num4] = array2[num4 - 1];
						}
						array[0] = text7;
						text2 = (array2[0] = text6 + string.Format("{0,3:f0}", num));
					}
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					binaryWriter.Write(text2.ToCharArray());
				}
				else
				{
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					if (new string(binaryReader.ReadChars(7)).Trim().Length > 0)
					{
						fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
						binaryWriter.Write("".PadRight(7).ToCharArray());
					}
				}
			}
			fileStream.Close();
		}

		private void SortFile(string SourceFileName)
		{
			ArrayList arrayList = new ArrayList();
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamReader streamReader = new StreamReader(SourceFileName, Encoding.ASCII))
			{
				while (!streamReader.EndOfStream)
				{
					arrayList.Add(streamReader.ReadLine());
				}
			}
			arrayList.Sort();
			using (StreamWriter streamWriter = new StreamWriter(Path.GetDirectoryName(SourceFileName) + "\\S_" + Path.GetFileName(SourceFileName)))
			{
				for (int i = 0; i < arrayList.Count; i++)
				{
					streamWriter.WriteLine(arrayList[i]!.ToString());
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void ListDuplicates(string SourceFileName)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			LunarObservations.ShowDuplicates();
			LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Clear();
			using (StreamReader streamReader = new StreamReader(SourceFileName, Encoding.ASCII))
			{
				string text = streamReader.ReadLine();
				do
				{
					string text2 = streamReader.ReadLine();
					if (text2.Substring(0, 25) == text.Substring(0, 25) && text2.Substring(81, 8) == text.Substring(81, 8) && text2.Substring(92, 7) == text.Substring(92, 7))
					{
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)text);
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)text2);
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)"");
					}
					text = text2;
				}
				while (!streamReader.EndOfStream);
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGZip_Click(object sender, EventArgs e)
		{
			GZipArchive();
		}

		private bool GZipArchive()
		{
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0099: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			string path = Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt";
			string path2 = Utilities.AppPath + "\\Resource Files\\LunarOcc.txt.gz";
			try
			{
				FileStream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read);
				byte[] array = new byte[fileStream.Length];
				if (fileStream.Read(array, 0, array.Length) != array.Length)
				{
					fileStream.Close();
					MessageBox.Show("GZip failed: Unable to read data from file", "GZip fail", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return false;
				}
				fileStream.Close();
				GZipStream gZipStream = new GZipStream(new FileStream(path2, FileMode.Create, FileAccess.Write), CompressionMode.Compress, leaveOpen: false);
				gZipStream.Write(array, 0, array.Length);
				gZipStream.Close();
			}
			catch
			{
				MessageBox.Show("GZip failed", "GZip fail", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			MessageBox.Show("LunarOcc.txt.gz created", "GZip success", (MessageBoxButtons)0, (MessageBoxIcon)48);
			return true;
		}

		private void cmdSort_Click(object sender, EventArgs e)
		{
			string text = updnYear.get_Value().ToString();
			ArrayList arrayList = new ArrayList();
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt", Encoding.ASCII);
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\Vizier Archive " + text + "+Sorted.txt");
			string text2;
			while (true)
			{
				text2 = streamReader.ReadLine();
				if (text2.Substring(0, 4) == text)
				{
					break;
				}
				streamWriter.Write(text2 + "\n");
			}
			arrayList.Add(text2);
			do
			{
				arrayList.Add(streamReader.ReadLine());
			}
			while (!streamReader.EndOfStream);
			arrayList.Sort();
			for (int i = 0; i < arrayList.Count; i++)
			{
				streamWriter.Write(arrayList[i]!.ToString() + "\n");
			}
		}

		private void cmdReduceAll_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("You are about to reduce all the Lunar Archive files. This may take several hours.\r\n\r\nDo you want to procede?", "Reduce all lunar archives", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Moon", "Moon*.bin");
				for (int i = 0; i < files.Length; i++)
				{
					File.Delete(files[i]);
				}
				files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files", "Archive Observations *.dat");
				for (int i = 0; i < files.Length; i++)
				{
					LunarObservations.ReduceArchiveOccultationFile(files[i], UsingSiteID: false, Settings.Default.ArchiveGrazesExcludeStartEnd, Settings.Default.ArchiveGrazesExcludeInvalid);
				}
			}
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_0a1a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a24: Expected O, but got Unknown
			//IL_0b31: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3b: Expected O, but got Unknown
			//IL_0beb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf5: Expected O, but got Unknown
			panelCombine = new Panel();
			cmdReduceAll = new Button();
			updnGrazeRenumberYear = new NumericUpDown();
			groupBox9 = new GroupBox();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			updnYear = new NumericUpDown();
			cmdSort = new Button();
			cmdGZip = new Button();
			chkVizierLimitTo1960 = new CheckBox();
			cmdVizier = new Button();
			label63 = new Label();
			label56 = new Label();
			txtEmailSignatureName = new TextBox();
			label40 = new Label();
			checkBox1 = new CheckBox();
			checkBox2 = new CheckBox();
			cmdGenerateGrazeIndex = new Button();
			label36 = new Label();
			cmbObservationsSource = new ComboBox();
			cmdProcessfromCoordinators = new Button();
			((Control)panelCombine).SuspendLayout();
			((ISupportInitialize)updnGrazeRenumberYear).BeginInit();
			((Control)groupBox9).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)this).SuspendLayout();
			((Control)panelCombine).get_Controls().Add((Control)(object)cmdReduceAll);
			((Control)panelCombine).get_Controls().Add((Control)(object)updnGrazeRenumberYear);
			((Control)panelCombine).get_Controls().Add((Control)(object)groupBox9);
			((Control)panelCombine).get_Controls().Add((Control)(object)label63);
			((Control)panelCombine).get_Controls().Add((Control)(object)label56);
			((Control)panelCombine).get_Controls().Add((Control)(object)txtEmailSignatureName);
			((Control)panelCombine).get_Controls().Add((Control)(object)label40);
			((Control)panelCombine).get_Controls().Add((Control)(object)checkBox1);
			((Control)panelCombine).get_Controls().Add((Control)(object)checkBox2);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmdGenerateGrazeIndex);
			((Control)panelCombine).get_Controls().Add((Control)(object)label36);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmbObservationsSource);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmdProcessfromCoordinators);
			((Control)panelCombine).set_Location(new Point(12, 38));
			((Control)panelCombine).set_Name("panelCombine");
			((Control)panelCombine).set_Size(new Size(738, 439));
			((Control)panelCombine).set_TabIndex(70);
			((Control)cmdReduceAll).set_Location(new Point(528, 131));
			((Control)cmdReduceAll).set_Name("cmdReduceAll");
			((Control)cmdReduceAll).set_Size(new Size(124, 41));
			((Control)cmdReduceAll).set_TabIndex(80);
			((Control)cmdReduceAll).set_Text("Reduce ALL archive files");
			((ButtonBase)cmdReduceAll).set_UseVisualStyleBackColor(true);
			((Control)cmdReduceAll).add_Click((EventHandler)cmdReduceAll_Click);
			((Control)updnGrazeRenumberYear).set_Location(new Point(232, 188));
			updnGrazeRenumberYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnGrazeRenumberYear.set_Minimum(new decimal(new int[4] { 1950, 0, 0, 0 }));
			((Control)updnGrazeRenumberYear).set_Name("updnGrazeRenumberYear");
			((Control)updnGrazeRenumberYear).set_Size(new Size(54, 20));
			((Control)updnGrazeRenumberYear).set_TabIndex(79);
			updnGrazeRenumberYear.set_Value(new decimal(new int[4] { 2010, 0, 0, 0 }));
			((Control)groupBox9).get_Controls().Add((Control)(object)label3);
			((Control)groupBox9).get_Controls().Add((Control)(object)label2);
			((Control)groupBox9).get_Controls().Add((Control)(object)label1);
			((Control)groupBox9).get_Controls().Add((Control)(object)updnYear);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdSort);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdGZip);
			((Control)groupBox9).get_Controls().Add((Control)(object)chkVizierLimitTo1960);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdVizier);
			((Control)groupBox9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox9).set_Location(new Point(137, 297));
			((Control)groupBox9).set_Name("groupBox9");
			((Control)groupBox9).set_Size(new Size(527, 115));
			((Control)groupBox9).set_TabIndex(78);
			groupBox9.set_TabStop(false);
			((Control)groupBox9).set_Text("Create Lunar Archive for Vizier");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(386, 18));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(44, 13));
			((Control)label3).set_TabIndex(84);
			((Control)label3).set_Text("Step 3");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(217, 19));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(44, 13));
			((Control)label2).set_TabIndex(83);
			((Control)label2).set_Text("Step 2");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(59, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(44, 13));
			((Control)label1).set_TabIndex(82);
			((Control)label1).set_Text("Step 1");
			((Control)updnYear).set_Location(new Point(213, 62));
			updnYear.set_Maximum(new decimal(new int[4] { 2030, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1980, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(52, 20));
			((Control)updnYear).set_TabIndex(81);
			updnYear.set_Value(new decimal(new int[4] { 2012, 0, 0, 0 }));
			((Control)cmdSort).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSort).set_Location(new Point(179, 35));
			((Control)cmdSort).set_Name("cmdSort");
			((Control)cmdSort).set_Size(new Size(121, 24));
			((Control)cmdSort).set_TabIndex(80);
			((Control)cmdSort).set_Text("Sort all years from");
			((ButtonBase)cmdSort).set_UseVisualStyleBackColor(true);
			((Control)cmdSort).add_Click((EventHandler)cmdSort_Click);
			((Control)cmdGZip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGZip).set_Location(new Point(348, 34));
			((Control)cmdGZip).set_Name("cmdGZip");
			((Control)cmdGZip).set_Size(new Size(120, 27));
			((Control)cmdGZip).set_TabIndex(79);
			((Control)cmdGZip).set_Text("GZip the Archive file");
			((ButtonBase)cmdGZip).set_UseVisualStyleBackColor(true);
			((Control)cmdGZip).add_Click((EventHandler)cmdGZip_Click);
			((Control)chkVizierLimitTo1960).set_AutoSize(true);
			((Control)chkVizierLimitTo1960).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkVizierLimitTo1960).set_Location(new Point(9, 83));
			((Control)chkVizierLimitTo1960).set_Name("chkVizierLimitTo1960");
			((Control)chkVizierLimitTo1960).set_Size(new Size(329, 17));
			((Control)chkVizierLimitTo1960).set_TabIndex(78);
			((Control)chkVizierLimitTo1960).set_Text("Create 'Vizier Archive pre 1961' (for computing deltaT correction)");
			((ButtonBase)chkVizierLimitTo1960).set_UseVisualStyleBackColor(true);
			((Control)chkVizierLimitTo1960).set_Visible(false);
			((Control)cmdVizier).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdVizier).set_Location(new Point(21, 33));
			((Control)cmdVizier).set_Name("cmdVizier");
			((Control)cmdVizier).set_Size(new Size(120, 44));
			((Control)cmdVizier).set_TabIndex(77);
			((Control)cmdVizier).set_Text("Create Archive for Vizier");
			((ButtonBase)cmdVizier).set_UseVisualStyleBackColor(true);
			((Control)cmdVizier).add_Click((EventHandler)cmdVizier_Click);
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(151, 175));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(132, 26));
			((Control)label63).set_TabIndex(76);
			((Control)label63).set_Text("Grazes are renumbered for\r\nyears on && after");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Location(new Point(358, 69));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(229, 13));
			((Control)label56).set_TabIndex(75);
			((Control)label56).set_Text("Signature name to use on Email to coordinators");
			((Control)txtEmailSignatureName).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "SignatureBlockName", true, (DataSourceUpdateMode)1));
			((Control)txtEmailSignatureName).set_Location(new Point(358, 85));
			((Control)txtEmailSignatureName).set_Name("txtEmailSignatureName");
			((Control)txtEmailSignatureName).set_Size(new Size(180, 20));
			((Control)txtEmailSignatureName).set_TabIndex(74);
			((Control)txtEmailSignatureName).set_Text(Settings.Default.SignatureBlockName);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(134, 55));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(75, 13));
			((Control)label40).set_TabIndex(73);
			((Control)label40).set_Text("in Reductions:");
			((Control)checkBox1).set_AutoSize(true);
			checkBox1.set_Checked(Settings.Default.ArchiveGrazesExcludeInvalid);
			checkBox1.set_CheckState((CheckState)1);
			((Control)checkBox1).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeInvalid", true, (DataSourceUpdateMode)1));
			((Control)checkBox1).set_Location(new Point(137, 71));
			((Control)checkBox1).set_Name("checkBox1");
			((Control)checkBox1).set_Size(new Size(160, 17));
			((Control)checkBox1).set_TabIndex(72);
			((Control)checkBox1).set_Text("Exclude invalid observations");
			((ButtonBase)checkBox1).set_UseVisualStyleBackColor(true);
			((Control)checkBox2).set_AutoSize(true);
			checkBox2.set_Checked(Settings.Default.ArchiveGrazesExcludeStartEnd);
			checkBox2.set_CheckState((CheckState)1);
			((Control)checkBox2).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeStartEnd", true, (DataSourceUpdateMode)1));
			((Control)checkBox2).set_Location(new Point(137, 88));
			((Control)checkBox2).set_Name("checkBox2");
			((Control)checkBox2).set_Size(new Size(177, 17));
			((Control)checkBox2).set_TabIndex(71);
			((Control)checkBox2).set_Text("Exclude Start/End graze events");
			((ButtonBase)checkBox2).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateGrazeIndex).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGenerateGrazeIndex).set_Location(new Point(341, 131));
			((Control)cmdGenerateGrazeIndex).set_Name("cmdGenerateGrazeIndex");
			((Control)cmdGenerateGrazeIndex).set_Size(new Size(124, 41));
			((Control)cmdGenerateGrazeIndex).set_TabIndex(70);
			((Control)cmdGenerateGrazeIndex).set_Text("Create graze index file\r\nfrom main files");
			((ButtonBase)cmdGenerateGrazeIndex).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateGrazeIndex).add_Click((EventHandler)cmdGenerateGrazeIndex_Click);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(141, 7));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(145, 13));
			((Control)label36).set_TabIndex(69);
			((Control)label36).set_Text("select source of observations");
			((ListControl)cmbObservationsSource).set_FormattingEnabled(true);
			cmbObservationsSource.get_Items().AddRange(new object[10] { "A    New observation reports", "H    RGO grazing occultations, OCR'd from RGO Bulletin\rI  ILOC computer records", "I    ILOC computer records", "M    Miscellaneous sources (grazes before 1960)", "N    S. Newcombe: Researches on the Motion of the Moon (APAE, 1878)", "O    grazes in the files of Occult", "P    re-processed ILOC reports after 2002", "R    RGO computer records, ordinary occultations ", "S    Graze reports collected by Mitsuru Soma - up to 2007.", "X    ILOC computer records after 2000, where no site code was allocated" });
			((Control)cmbObservationsSource).set_Location(new Point(137, 22));
			((Control)cmbObservationsSource).set_Name("cmbObservationsSource");
			((Control)cmbObservationsSource).set_Size(new Size(400, 21));
			((Control)cmbObservationsSource).set_TabIndex(68);
			((Control)cmdProcessfromCoordinators).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdProcessfromCoordinators).set_Location(new Point(158, 131));
			((Control)cmdProcessfromCoordinators).set_Name("cmdProcessfromCoordinators");
			((Control)cmdProcessfromCoordinators).set_Size(new Size(124, 41));
			((Control)cmdProcessfromCoordinators).set_TabIndex(59);
			((Control)cmdProcessfromCoordinators).set_Text("Process files from\r\ncoordinators");
			((ButtonBase)cmdProcessfromCoordinators).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessfromCoordinators).add_Click((EventHandler)cmdProcessfromCoordinators_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(760, 484));
			((Control)this).get_Controls().Add((Control)(object)panelCombine);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("GlobalMaintenance");
			((Control)this).set_Text("Process lunar archive files from regional coordinators");
			((Form)this).add_Load((EventHandler)GlobalMaintenance_Load);
			((Control)panelCombine).ResumeLayout(false);
			((Control)panelCombine).PerformLayout();
			((ISupportInitialize)updnGrazeRenumberYear).EndInit();
			((Control)groupBox9).ResumeLayout(false);
			((Control)groupBox9).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((Control)this).ResumeLayout(false);
		}
	}
}
