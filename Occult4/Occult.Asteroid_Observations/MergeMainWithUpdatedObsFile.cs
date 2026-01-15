using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class MergeMainWithUpdatedObsFile : Form
	{
		private double JDstart = 2415020.5;

		private double JDend = 2415020.5;

		private double JDmergeStart = 2415020.5;

		private double JDmergeEnd = 2415020.5;

		internal static AllEvents HistoricalFile = new AllEvents();

		internal static AllEvents FileToMerge = new AllEvents();

		internal static AllEvents MergedFile = new AllEvents();

		private static AllEvents FileOfQualityEvents = new AllEvents();

		private string ToMerge_FileName = "";

		private string ToMerge_FileNameWithPath = "";

		private string QualityFileRootProcessed = Utilities.AppPath + "\\Import_Export\\Processed";

		private string QualityFileRootNotAdded = Utilities.AppPath + "\\Import_Export\\NotAdded";

		private IContainer components;

		private NumericUpDown updnYear;

		private NumericUpDown updnStartMonth;

		private Label label2;

		private NumericUpDown updnEndMonth;

		private Label label3;

		private NumericUpDown updnEndDay;

		private NumericUpDown updnStartDay;

		private Label label4;

		private Label label5;

		private Button cmdMerge;

		private OpenFileDialog openFileDialog1;

		private Label label6;

		private Label label7;

		private Label label8;

		private GroupBox grpSetDates;

		private Button cmdCreateFileToMerge;

		private Label label10;

		private Label label9;

		private Label label1;

		private GroupBox groupBox1;

		private ListBox lstMergeFiles;

		private Label label11;

		private Label label15;

		private Label label14;

		private Label label13;

		private Label label12;

		private TextBox txtEnd;

		private TextBox txtStart;

		private TextBox txtYear;

		private Label label17;

		private Label label16;

		private Panel panel1;

		private Label label18;

		private Label label19;

		private Panel panel2;

		private Panel panel3;

		private Label label22;

		private ListBox lstQualityFiles;

		private Label label20;

		private Label label21;

		private CheckedListBox chkLstEventsToAdd;

		private Button cmdAddQuality;

		private Label label23;

		private Label label24;

		private Label label26;

		private Label label25;

		private Label label27;

		private Label label29;

		private Label label28;

		private TextBox txtSelected;

		public MergeMainWithUpdatedObsFile()
		{
			InitializeComponent();
		}

		private void MergeMainWithUpdatedObsFile_Load(object sender, EventArgs e)
		{
			Data_and_Plots.ShowEditor();
			Data_and_Plots.ShowPlotForm();
			updnEndDay.set_Value((decimal)Utilities.MaximumDaysInMonth((int)updnYear.get_Value(), (int)updnEndMonth.get_Value()));
			SetStartEndJD();
			PopulateListOfMergeFiles();
			PopulateListOfQualityFiles();
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			if (updnEndMonth.get_Value() < updnStartMonth.get_Value())
			{
				updnEndMonth.set_Value(updnStartMonth.get_Value());
			}
			SetStartEndJD();
		}

		private void SetStartEndJD()
		{
			string text = "";
			if (updnYear.get_Value() == 1979m)
			{
				JDstart = Utilities.JD_from_Date(1800, 1, 1.0);
				JDend = Utilities.JD_from_Date(1979, 12, 31.0);
				text = "1979_01_to_12";
			}
			else
			{
				JDstart = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
				JDend = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnEndMonth.get_Value(), (int)updnEndDay.get_Value()) + 1.0;
				text = updnYear.get_Value() + "_" + updnStartMonth.get_Value().ToString().PadLeft(2, '0') + "_to_" + updnEndMonth.get_Value().ToString().PadLeft(2, '0');
			}
			ToMerge_FileName = Utilities.AsteroidObservationsFile.Substring(0, Utilities.AsteroidObservationsFile.IndexOf(".xml")) + "_" + text;
			ToMerge_FileNameWithPath = Utilities.AppPath + "\\Import_Export\\" + ToMerge_FileName + ".xml";
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			updnEndDay.set_Value((decimal)Utilities.MaximumDaysInMonth((int)updnYear.get_Value(), (int)updnEndMonth.get_Value()));
			SetStartEndJD();
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			updnEndDay.set_Value((decimal)Utilities.MaximumDaysInMonth((int)updnYear.get_Value(), (int)updnEndMonth.get_Value()));
			SetStartEndJD();
		}

		private void cmdCreateFileToMerge_Click(object sender, EventArgs e)
		{
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_002d: Invalid comparison between Unknown and I4
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006a: Invalid comparison between Unknown and I4
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("You are about to create a file:\r\n        " + ToMerge_FileName + ".xml\r\nin the   Import_Export  folder of Occult\r\n\r\nThis file is a copy of your current 'Asteroid~Observations.xml' file.\r\n\r\nOnly edits that have been made to events for the year that \r\nare within the specified month range will be used when the \r\nfile is used to merge edited obsevations with the main \r\nfile of observations\r\n\r\nHave you specified the correct Year and range of Months?", "Confirm date range", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7 && (!File.Exists(ToMerge_FileNameWithPath) || (int)MessageBox.Show("The file  " + ToMerge_FileName + ".xml  Exists. \r\n\r\nDo you want to overwrite that file?", "Confirm file overwrite", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7))
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: false, ToMerge_FileNameWithPath);
				PopulateListOfMergeFiles();
				MessageBox.Show("The file " + ToMerge_FileName + ".xml has been created in the Import_Export folder", "File created", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void lstMergeFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetOutputDates();
		}

		private void SetOutputDates()
		{
			if (((ListControl)lstMergeFiles).get_SelectedIndex() < 0)
			{
				return;
			}
			string[] array = lstMergeFiles.get_Items().get_Item(((ListControl)lstMergeFiles).get_SelectedIndex()).ToString()!.Split(new char[1] { '_' });
			if (array.Length == 4)
			{
				((Control)txtYear).set_Text(array[0]);
				((Control)txtStart).set_Text(Utilities.ShortMonths[int.Parse(array[1])]);
				((Control)txtEnd).set_Text(Utilities.ShortMonths[int.Parse(array[3])]);
				int num = int.Parse(array[0]);
				if (num == 1979)
				{
					num = 1800;
				}
				JDmergeStart = Utilities.JD_from_Date(num, int.Parse(array[1]), 1.0);
				int num2 = int.Parse(array[0]);
				int num3 = int.Parse(array[3]) + 1;
				if (num3 > 12)
				{
					num2++;
					num3 = 1;
				}
				JDmergeEnd = Utilities.JD_from_Date(num2, num3, 1.0);
			}
		}

		private void PopulateListOfMergeFiles()
		{
			lstMergeFiles.get_Items().Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Import_Export").GetFiles("Asteroid~Observations*.xml");
			if (files.Length >= 0)
			{
				FileInfo[] array = files;
				foreach (FileInfo fileInfo in array)
				{
					if (fileInfo.Name.Substring(35) == ".xml")
					{
						lstMergeFiles.get_Items().Add((object)fileInfo.Name.Substring(22, 13));
					}
				}
				if (lstMergeFiles.get_Items().get_Count() >= 0)
				{
					lstMergeFiles.set_SelectedItem((object)0);
					SetOutputDates();
					((Control)cmdMerge).set_Enabled(true);
				}
			}
			else
			{
				lstMergeFiles.get_Items().Add((object)"None available");
				TextBox obj = txtYear;
				TextBox obj2 = txtStart;
				string text;
				((Control)txtEnd).set_Text(text = "--");
				string text2;
				((Control)obj2).set_Text(text2 = text);
				((Control)obj).set_Text(text2);
				((Control)cmdMerge).set_Enabled(false);
			}
		}

		private void cmdMerge_Click(object sender, EventArgs e)
		{
			//IL_01ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Invalid comparison between Unknown and I4
			HistoricalFile = new AllEvents();
			FileToMerge = new AllEvents();
			MergedFile = new AllEvents();
			SetOutputDates();
			string text = lstMergeFiles.get_Items().get_Item(((ListControl)lstMergeFiles).get_SelectedIndex()).ToString() + ".xml";
			HistoricalFile.ReadObservationsFile(HistoricalFile: false, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile);
			string text2 = Utilities.AppPath + "\\Import_Export\\Asteroid~Observations_" + text;
			FileToMerge.ReadObservationsFile(HistoricalFile: false, text2);
			int num = 0;
			for (int i = 0; i < HistoricalFile.OccEvents.Count; i++)
			{
				num = i;
				double eventJD = HistoricalFile.OccEvents[i].EventJD;
				if (!(eventJD <= JDmergeStart))
				{
					break;
				}
				LinesForAnEvent linesForAnEvent = new LinesForAnEvent();
				linesForAnEvent = HistoricalFile.OccEvents[i];
				MergedFile.OccEvents.Add(linesForAnEvent);
			}
			for (int j = 0; j < FileToMerge.OccEvents.Count; j++)
			{
				double eventJD = FileToMerge.OccEvents[j].EventJD;
				if ((eventJD > JDmergeStart) & (eventJD < JDmergeEnd))
				{
					LinesForAnEvent linesForAnEvent = new LinesForAnEvent();
					linesForAnEvent = FileToMerge.OccEvents[j];
					MergedFile.OccEvents.Add(linesForAnEvent);
				}
			}
			for (int k = num; k < HistoricalFile.OccEvents.Count; k++)
			{
				double eventJD = HistoricalFile.OccEvents[k].EventJD;
				if (eventJD >= JDmergeEnd)
				{
					LinesForAnEvent linesForAnEvent = new LinesForAnEvent();
					linesForAnEvent = HistoricalFile.OccEvents[k];
					MergedFile.OccEvents.Add(linesForAnEvent);
				}
			}
			if ((int)MessageBox.Show("The observations from the two files have been merged \r\nin memory, and are now ready to be saved.\r\n\r\nDo you want to save the merged set of observations?\r\n\r\nThe existing Asteroid~Observations.xml file will be renamed to\r\n\r\nAsteroid~Observations_Pre_" + text, "Confirm write", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				File.Move(Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile.Replace(".xml", "_Pre_" + text));
				MergedFile.WriteHistoricalObservationsFile(HistoricalFile: true, "");
				File.Move(text2, text2.Replace(".xml", "_done.xml"));
			}
			HistoricalFile.OccEvents.Clear();
			FileToMerge.OccEvents.Clear();
			MergedFile.OccEvents.Clear();
			PopulateListOfMergeFiles();
		}

		private void PopulateListOfQualityFiles()
		{
			lstQualityFiles.get_Items().Clear();
			((ObjectCollection)chkLstEventsToAdd.get_Items()).Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Import_Export").GetFiles(ObservationsEditor.Quality_FileNameBase + "*.xml");
			if (files.Length >= 0)
			{
				FileInfo[] array = files;
				foreach (FileInfo obj in array)
				{
					int num = obj.Name.IndexOf(".xml", ObservationsEditor.Quality_FileNameBase.Length);
					string text = obj.Name.Substring(ObservationsEditor.Quality_FileNameBase.Length, num - ObservationsEditor.Quality_FileNameBase.Length);
					lstQualityFiles.get_Items().Add((object)text);
				}
				if (lstQualityFiles.get_Items().get_Count() <= 0)
				{
					return;
				}
				lstQualityFiles.set_SelectedItem((object)0);
				ReadQualityEvents(0);
				((Control)cmdAddQuality).set_Enabled(true);
			}
			else
			{
				lstQualityFiles.get_Items().Add((object)"None available");
				((Control)cmdAddQuality).set_Enabled(false);
			}
			((Control)txtSelected).set_Text(lstQualityFiles.get_Items().get_Item(0).ToString());
		}

		private void lstQualityFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			ReadQualityEvents(((ListControl)lstQualityFiles).get_SelectedIndex());
			((Control)txtSelected).set_Text(lstQualityFiles.get_Items().get_Item(((ListControl)lstQualityFiles).get_SelectedIndex()).ToString());
		}

		private void ReadQualityEvents(int FileNumber)
		{
			Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: false, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile);
			FileOfQualityEvents = new AllEvents();
			((ObjectCollection)chkLstEventsToAdd.get_Items()).Clear();
			ObservationsEditor.Quality_FileName = lstQualityFiles.get_Items().get_Item(FileNumber).ToString();
			string singleFileName = ObservationsEditor.QualityFileRoot + ObservationsEditor.Quality_FileNameBase + ObservationsEditor.Quality_FileName + ".xml";
			FileOfQualityEvents.ReadObservationsFile(HistoricalFile: false, singleFileName);
			for (int i = 0; i < FileOfQualityEvents.OccEvents.Count; i++)
			{
				((ObjectCollection)chkLstEventsToAdd.get_Items()).Add((object)FileOfQualityEvents.OccEvents[i].IndexLine);
			}
			for (int j = 0; j < FileOfQualityEvents.OccEvents.Count; j++)
			{
				if (!CheckIfInHistoricalFile(j))
				{
					chkLstEventsToAdd.SetItemCheckState(j, (CheckState)1);
				}
				else
				{
					chkLstEventsToAdd.SetItemCheckState(j, (CheckState)0);
				}
				if (!FileOfQualityEvents.OccEvents[j].ObsFileisCurrentVersion)
				{
					chkLstEventsToAdd.SetItemCheckState(j, (CheckState)0);
				}
			}
		}

		private static bool CheckIfInHistoricalFile(int QualityRecordNum)
		{
			try
			{
				Utilities.Date_from_JD(FileOfQualityEvents.OccEvents[QualityRecordNum].EventJD, out var Year, out var Month, out var day);
				string value = Year + Month.ToString().PadLeft(3) + Convert.ToInt16(Math.Floor(day)).ToString().PadLeft(3);
				string text = FileOfQualityEvents.OccEvents[QualityRecordNum].AstName.Trim();
				string text2 = FileOfQualityEvents.OccEvents[QualityRecordNum].StarID.Trim().Replace("-coords ", "");
				for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[i].IndexLine.Contains(value))
					{
						Data_and_Plots.Historical_AllEvents.GetAsteroidID(i, out var _, out var AsteroidName);
						if (text == AsteroidName && text2 == Data_and_Plots.Historical_AllEvents.GetStarID(i).Trim().Replace("-coords ", ""))
						{
							return true;
						}
					}
				}
			}
			catch
			{
			}
			return false;
		}

		private void cmdAddQuality_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c2: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This will\r\n*  Write the checked events to the observations file currently\r\n     in Memory\r\n*  The updated observations file will not be saved at this time\r\n\r\nDo you want to continue?", "Confirm addition of events", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			HistoricalFile.ReadObservationsFile(HistoricalFile: false, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile);
			for (int i = 0; i < FileOfQualityEvents.OccEvents.Count; i++)
			{
				if (chkLstEventsToAdd.GetItemChecked(i))
				{
					Data_and_Plots.Historical_AllEvents.OccEvents.Add(FileOfQualityEvents.OccEvents[i]);
				}
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Sort();
			if ((int)MessageBox.Show("The checked events have been added in memory, and are now ready to be saved.\r\n\r\n*  Do you want to save the updated set of observations?\r\n\r\n*  The existing Asteroid~Observations.xml file will be\r\n  renamed to   Asteroid~Observations_Pre_" + ObservationsEditor.Quality_FileNameBase + ObservationsEditor.Quality_FileName + ".xml\r\n\r\nNo   will reload the observations file without the additions", "Confirm write", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				File.Move(Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile.Replace(".xml", "_Pre_" + ObservationsEditor.Quality_FileName + ".xml"));
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
				if (!Directory.Exists(QualityFileRootNotAdded))
				{
					Directory.CreateDirectory(QualityFileRootNotAdded);
				}
				for (int j = 0; j < FileOfQualityEvents.OccEvents.Count; j++)
				{
					if (chkLstEventsToAdd.GetItemChecked(j))
					{
						continue;
					}
					using StreamWriter streamWriter = new StreamWriter(QualityFileRootNotAdded + "\\" + ObservationsEditor.Quality_FileNameBase + ObservationsEditor.Quality_FileName + string.Format("_#{0,1}.xml", j + 1));
					streamWriter.WriteLine(Tags.TagFileStart);
					streamWriter.WriteLine("  <FileVersion>" + Tags.ObservationsFileVersion + "</FileVersion>");
					for (int k = 0; k < FileOfQualityEvents.OccEvents[j].Lines.Count; k++)
					{
						streamWriter.WriteLine(FileOfQualityEvents.OccEvents[j].Lines[k]);
					}
					streamWriter.WriteLine(Tags.TagFileEnd);
				}
				if (!Directory.Exists(QualityFileRootProcessed))
				{
					Directory.CreateDirectory(QualityFileRootProcessed);
				}
				File.Move(ObservationsEditor.QualityFileRoot + "\\" + ObservationsEditor.Quality_FileNameBase + ObservationsEditor.Quality_FileName + ".xml", QualityFileRootProcessed + "\\" + ObservationsEditor.Quality_FileNameBase + ObservationsEditor.Quality_FileName + "_done.xml");
			}
			else
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: false, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile);
			}
			PopulateListOfQualityFiles();
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MergeMainWithUpdatedObsFile));
			updnYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			label2 = new Label();
			updnEndMonth = new NumericUpDown();
			label3 = new Label();
			updnEndDay = new NumericUpDown();
			updnStartDay = new NumericUpDown();
			label4 = new Label();
			label5 = new Label();
			cmdMerge = new Button();
			openFileDialog1 = new OpenFileDialog();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			grpSetDates = new GroupBox();
			panel1 = new Panel();
			label10 = new Label();
			label9 = new Label();
			label1 = new Label();
			label18 = new Label();
			cmdCreateFileToMerge = new Button();
			groupBox1 = new GroupBox();
			label19 = new Label();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			label14 = new Label();
			label13 = new Label();
			label12 = new Label();
			txtEnd = new TextBox();
			txtStart = new TextBox();
			txtYear = new TextBox();
			label11 = new Label();
			lstMergeFiles = new ListBox();
			panel2 = new Panel();
			panel3 = new Panel();
			txtSelected = new TextBox();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			label25 = new Label();
			label24 = new Label();
			label23 = new Label();
			chkLstEventsToAdd = new CheckedListBox();
			cmdAddQuality = new Button();
			label22 = new Label();
			lstQualityFiles = new ListBox();
			label20 = new Label();
			label21 = new Label();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndDay).BeginInit();
			((ISupportInitialize)updnStartDay).BeginInit();
			((Control)grpSetDates).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)updnYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnYear).set_Location(new Point(75, 141));
			updnYear.set_Maximum(new decimal(new int[4] { 2140, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1979, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(48, 20));
			((Control)updnYear).set_TabIndex(2);
			updnYear.set_Value(new decimal(new int[4] { 1979, 0, 0, 0 }));
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)updnStartMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnStartMonth).set_Location(new Point(275, 139));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnStartMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(34, 20));
			((Control)updnStartMonth).set_TabIndex(3);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(166, 141));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(102, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Start month and day");
			((Control)updnEndMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnEndMonth).set_Location(new Point(275, 165));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnEndMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(34, 20));
			((Control)updnEndMonth).set_TabIndex(6);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(169, 167));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(99, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("End month and day");
			((Control)updnEndDay).set_Enabled(false);
			((Control)updnEndDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnEndDay).set_Location(new Point(318, 165));
			updnEndDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnEndDay.set_Minimum(new decimal(new int[4] { 28, 0, 0, 0 }));
			((Control)updnEndDay).set_Name("updnEndDay");
			((Control)updnEndDay).set_Size(new Size(36, 20));
			((Control)updnEndDay).set_TabIndex(9);
			updnEndDay.set_Value(new decimal(new int[4] { 28, 0, 0, 0 }));
			((Control)updnStartDay).set_Enabled(false);
			((Control)updnStartDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnStartDay).set_Location(new Point(320, 139));
			updnStartDay.set_Maximum(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStartDay).set_Name("updnStartDay");
			((Control)updnStartDay).set_Size(new Size(36, 20));
			((Control)updnStartDay).set_TabIndex(10);
			updnStartDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(40, 143));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(11);
			((Control)label4).set_Text("Year");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(28, 164));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(106, 26));
			((Control)label5).set_TabIndex(12);
			((Control)label5).set_Text("For all events before \r\n1980, select 1979");
			((Control)cmdMerge).set_Enabled(false);
			((Control)cmdMerge).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMerge).set_Location(new Point(158, 84));
			((Control)cmdMerge).set_Name("cmdMerge");
			((Control)cmdMerge).set_Size(new Size(183, 32));
			((Control)cmdMerge).set_TabIndex(13);
			((Control)cmdMerge).set_Text("Create merged file of observations");
			((ButtonBase)cmdMerge).set_UseVisualStyleBackColor(true);
			((Control)cmdMerge).add_Click((EventHandler)cmdMerge_Click);
			((FileDialog)openFileDialog1).set_FileName("openFileDialog1");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(9, 0));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(408, 39));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text(componentResourceManager.GetString("label6.Text"));
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(9, 46));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(372, 26));
			((Control)label7).set_TabIndex(15);
			((Control)label7).set_Text("The edited file of observations will replace ALL entries in the main file that are \r\nwithin the period specified below on this form.");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(9, 79));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(363, 26));
			((Control)label8).set_TabIndex(16);
			((Control)label8).set_Text("The person creating a merge file containing edited observations MUST limit \r\nthe edits to months in a single year.");
			((Control)grpSetDates).set_BackColor(Color.LightCyan);
			((Control)grpSetDates).get_Controls().Add((Control)(object)panel1);
			((Control)grpSetDates).get_Controls().Add((Control)(object)label18);
			((Control)grpSetDates).get_Controls().Add((Control)(object)cmdCreateFileToMerge);
			((Control)grpSetDates).get_Controls().Add((Control)(object)label4);
			((Control)grpSetDates).get_Controls().Add((Control)(object)updnYear);
			((Control)grpSetDates).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)grpSetDates).get_Controls().Add((Control)(object)label2);
			((Control)grpSetDates).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)grpSetDates).get_Controls().Add((Control)(object)label5);
			((Control)grpSetDates).get_Controls().Add((Control)(object)label3);
			((Control)grpSetDates).get_Controls().Add((Control)(object)updnEndDay);
			((Control)grpSetDates).get_Controls().Add((Control)(object)updnStartDay);
			((Control)grpSetDates).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSetDates).set_Location(new Point(9, 120));
			((Control)grpSetDates).set_Name("grpSetDates");
			((Control)grpSetDates).set_Size(new Size(410, 230));
			((Control)grpSetDates).set_TabIndex(17);
			grpSetDates.set_TabStop(false);
			((Control)grpSetDates).set_Text("Create the file to be merged ");
			((Control)panel1).set_BackColor(Color.Honeydew);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(20, 19));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(370, 93));
			((Control)panel1).set_TabIndex(14);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(6, 53));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(338, 26));
			((Control)label10).set_TabIndex(2);
			((Control)label10).set_Text("where yyyy is the year, mm is the first edited month, and MM is the last \r\nedited month. Only edits made in this period will be merged");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(65, 32));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(236, 13));
			((Control)label9).set_TabIndex(1);
			((Control)label9).set_Text("Asteroid~Observations_yyyy_mm_to_MM");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(6, 3));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(358, 26));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("The file will be created in the Import_Export folder of Occult. The file name \r\nwill be in the form");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(75, 117));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(261, 13));
			((Control)label18).set_TabIndex(13);
			((Control)label18).set_Text("Set the date range for the data to be merged");
			((Control)cmdCreateFileToMerge).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateFileToMerge).set_Location(new Point(118, 192));
			((Control)cmdCreateFileToMerge).set_Name("cmdCreateFileToMerge");
			((Control)cmdCreateFileToMerge).set_Size(new Size(174, 32));
			((Control)cmdCreateFileToMerge).set_TabIndex(3);
			((Control)cmdCreateFileToMerge).set_Text("Create the file to be merged");
			((ButtonBase)cmdCreateFileToMerge).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateFileToMerge).add_Click((EventHandler)cmdCreateFileToMerge_Click);
			((Control)groupBox1).set_BackColor(Color.Lavender);
			((Control)groupBox1).get_Controls().Add((Control)(object)label19);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)label16);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)label13);
			((Control)groupBox1).get_Controls().Add((Control)(object)label12);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtEnd);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtStart);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtYear);
			((Control)groupBox1).get_Controls().Add((Control)(object)label11);
			((Control)groupBox1).get_Controls().Add((Control)(object)lstMergeFiles);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdMerge);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(9, 362));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(408, 174));
			((Control)groupBox1).set_TabIndex(19);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Combine the Main file and the Merge file");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(146, 21));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(207, 13));
			((Control)label19).set_TabIndex(25);
			((Control)label19).set_Text("Date range to be taken from the Merge file");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(118, 143));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(262, 13));
			((Control)label17).set_TabIndex(24);
			((Control)label17).set_Text("Asteroid~Observations_Pre_yyyy_mm_to_MM");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(118, 126));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(278, 13));
			((Control)label16).set_TabIndex(23);
			((Control)label16).set_Text("The existing Asteroid~Observations file will be renamed to");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(273, 55));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(16, 13));
			((Control)label15).set_TabIndex(22);
			((Control)label15).set_Text("to");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(293, 38));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(37, 13));
			((Control)label14).set_TabIndex(21);
			((Control)label14).set_Text("Month");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(232, 38));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(37, 13));
			((Control)label13).set_TabIndex(20);
			((Control)label13).set_Text("Month");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(169, 38));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(29, 13));
			((Control)label12).set_TabIndex(19);
			((Control)label12).set_Text("Year");
			((Control)txtEnd).set_Enabled(false);
			((Control)txtEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEnd).set_Location(new Point(291, 52));
			((Control)txtEnd).set_Name("txtEnd");
			((Control)txtEnd).set_Size(new Size(41, 20));
			((Control)txtEnd).set_TabIndex(18);
			txtEnd.set_TextAlign((HorizontalAlignment)2);
			((Control)txtStart).set_Enabled(false);
			((Control)txtStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStart).set_Location(new Point(230, 52));
			((Control)txtStart).set_Name("txtStart");
			((Control)txtStart).set_Size(new Size(41, 20));
			((Control)txtStart).set_TabIndex(17);
			txtStart.set_TextAlign((HorizontalAlignment)2);
			((Control)txtYear).set_Enabled(false);
			((Control)txtYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear).set_Location(new Point(163, 52));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(41, 20));
			((Control)txtYear).set_TabIndex(16);
			txtYear.set_TextAlign((HorizontalAlignment)2);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(2, 21));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(124, 13));
			((Control)label11).set_TabIndex(15);
			((Control)label11).set_Text("Unprocessed Merge files");
			((Control)lstMergeFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMergeFiles).set_FormattingEnabled(true);
			((Control)lstMergeFiles).set_Location(new Point(10, 39));
			((Control)lstMergeFiles).set_Name("lstMergeFiles");
			((Control)lstMergeFiles).set_Size(new Size(102, 121));
			((Control)lstMergeFiles).set_TabIndex(14);
			lstMergeFiles.add_SelectedIndexChanged((EventHandler)lstMergeFiles_SelectedIndexChanged);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)groupBox1);
			((Control)panel2).get_Controls().Add((Control)(object)grpSetDates);
			((Control)panel2).get_Controls().Add((Control)(object)label8);
			((Control)panel2).get_Controls().Add((Control)(object)label7);
			((Control)panel2).get_Controls().Add((Control)(object)label6);
			((Control)panel2).set_Location(new Point(4, 30));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(427, 544));
			((Control)panel2).set_TabIndex(20);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)txtSelected);
			((Control)panel3).get_Controls().Add((Control)(object)label29);
			((Control)panel3).get_Controls().Add((Control)(object)label28);
			((Control)panel3).get_Controls().Add((Control)(object)label27);
			((Control)panel3).get_Controls().Add((Control)(object)label26);
			((Control)panel3).get_Controls().Add((Control)(object)label25);
			((Control)panel3).get_Controls().Add((Control)(object)label24);
			((Control)panel3).get_Controls().Add((Control)(object)label23);
			((Control)panel3).get_Controls().Add((Control)(object)chkLstEventsToAdd);
			((Control)panel3).get_Controls().Add((Control)(object)cmdAddQuality);
			((Control)panel3).get_Controls().Add((Control)(object)label22);
			((Control)panel3).get_Controls().Add((Control)(object)lstQualityFiles);
			((Control)panel3).set_Location(new Point(455, 30));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(382, 544));
			((Control)panel3).set_TabIndex(21);
			((Control)txtSelected).set_Location(new Point(13, 286));
			((Control)txtSelected).set_Name("txtSelected");
			((TextBoxBase)txtSelected).set_ReadOnly(true);
			((Control)txtSelected).set_Size(new Size(102, 20));
			((Control)txtSelected).set_TabIndex(27);
			txtSelected.set_TextAlign((HorizontalAlignment)2);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(15, 269));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(98, 17));
			((Control)label29).set_TabIndex(26);
			((Control)label29).set_Text("Selected file");
			label29.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_ForeColor(Color.DarkOrchid);
			((Control)label28).set_Location(new Point(2, 39));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(378, 13));
			((Control)label28).set_TabIndex(25);
			((Control)label28).set_Text("Click on a Source file to list events. Wait while Main file checked");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_ForeColor(Color.DarkRed);
			((Control)label27).set_Location(new Point(1, 453));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(376, 17));
			((Control)label27).set_TabIndex(24);
			((Control)label27).set_Text("2. Add Checked events; create files for Unchecked");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_ForeColor(Color.MediumVioletRed);
			((Control)label26).set_Location(new Point(2, 99));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(322, 13));
			((Control)label26).set_TabIndex(23);
			((Control)label26).set_Text("Events not checked will be written out as separate files");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_ForeColor(Color.DarkGreen);
			((Control)label25).set_Location(new Point(2, 57));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(367, 39));
			((Control)label25).set_TabIndex(22);
			((Control)label25).set_Text("Events are Not Checked if:\r\n   they are already in the Main file; OR\r\n   the have the wrong file version (and must be separately read)");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_ForeColor(Color.DarkRed);
			((Control)label24).set_Location(new Point(2, 7));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(349, 17));
			((Control)label24).set_TabIndex(21);
			((Control)label24).set_Text("1. Select source file, and events within that file");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(133, 123));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(229, 17));
			((Control)label23).set_TabIndex(20);
			((Control)label23).set_Text("Quality-checked events to add");
			((Control)chkLstEventsToAdd).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkLstEventsToAdd).set_FormattingEnabled(true);
			((Control)chkLstEventsToAdd).set_Location(new Point(125, 142));
			((Control)chkLstEventsToAdd).set_Name("chkLstEventsToAdd");
			((Control)chkLstEventsToAdd).set_Size(new Size(245, 293));
			((Control)chkLstEventsToAdd).set_TabIndex(19);
			((Control)cmdAddQuality).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddQuality).set_Location(new Point(118, 479));
			((Control)cmdAddQuality).set_Name("cmdAddQuality");
			((Control)cmdAddQuality).set_Size(new Size(146, 44));
			((Control)cmdAddQuality).set_TabIndex(18);
			((Control)cmdAddQuality).set_Text("Add Quality-checked events");
			((ButtonBase)cmdAddQuality).set_UseVisualStyleBackColor(true);
			((Control)cmdAddQuality).add_Click((EventHandler)cmdAddQuality_Click);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(17, 123));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(94, 17));
			((Control)label22).set_TabIndex(17);
			((Control)label22).set_Text("Source files");
			label22.set_TextAlign(ContentAlignment.TopCenter);
			((Control)lstQualityFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstQualityFiles).set_FormattingEnabled(true);
			((Control)lstQualityFiles).set_Location(new Point(13, 142));
			((Control)lstQualityFiles).set_Name("lstQualityFiles");
			((Control)lstQualityFiles).set_Size(new Size(102, 121));
			((Control)lstQualityFiles).set_TabIndex(16);
			lstQualityFiles.add_SelectedIndexChanged((EventHandler)lstQualityFiles_SelectedIndexChanged);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(65, 7));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(260, 20));
			((Control)label20).set_TabIndex(22);
			((Control)label20).set_Text("Merged edited observation files");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(486, 7));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(317, 20));
			((Control)label21).set_TabIndex(23);
			((Control)label21).set_Text("Add Quality-checked events from a file");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.Ivory);
			((Form)this).set_ClientSize(new Size(845, 582));
			((Control)this).get_Controls().Add((Control)(object)label21);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_HelpButton(true);
			((Control)this).set_Name("MergeMainWithUpdatedObsFile");
			((Control)this).set_Text("Merge file with updated observations");
			((Form)this).add_Load((EventHandler)MergeMainWithUpdatedObsFile_Load);
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndDay).EndInit();
			((ISupportInitialize)updnStartDay).EndInit();
			((Control)grpSetDates).ResumeLayout(false);
			((Control)grpSetDates).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
