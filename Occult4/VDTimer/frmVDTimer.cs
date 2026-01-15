using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Occult;

namespace VDTimer
{
	public class frmVDTimer : Form
	{
		private const string _timeFormat = "yyyy-MM-dd  HH:mm:ss";

		private const int RecordingGap = 8;

		private Thread _timerThread;

		private volatile bool _timerEnabled;

		private string TimerSchedule = Utilities.AppPath + "\\Predictions\\Timer_schedule.txt";

		private IContainer components;

		private GroupBox grpSchedule;

		private Button btnEdit;

		private Button btnRemove;

		private Button btnAdd;

		private ListView lvwTimes;

		private ColumnHeader chStartTime;

		private ColumnHeader chStopTime;

		private Button btnClear;

		private Label lblCaptureWindowFoundDesc;

		private ColumnHeader chComment;

		private Label lblCaptureWindowFound;

		private Label label1;

		public CheckBox chkEnableTimer;

		private Button cmdRemoveAllOld;

		private Label lblEnabled;

		private ColumnHeader chCount;

		private Button cmdHelp;

		private Label label2;

		public frmVDTimer()
		{
			InitializeComponent();
		}

		private void frmVDTimer_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			lvwTimes.set_ListViewItemSorter((IComparer)new LVScheduleSort());
			LoadSchedule();
		}

		private void frmVDTimer_FormClosed(object sender, FormClosedEventArgs e)
		{
			if (_timerThread != null && _timerThread.IsAlive)
			{
				_timerEnabled = false;
				_timerThread.Join();
			}
			SaveSchedule();
		}

		private void btnAdd_Click(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Invalid comparison between Unknown and I4
			frmTimeInput frmTimeInput2 = new frmTimeInput("Add Time");
			frmTimeInput2.StartTime = DateTime.Now.AddSeconds(10.0);
			frmTimeInput2.StopTime = DateTime.Now.AddSeconds(20.0);
			if ((int)((Form)frmTimeInput2).ShowDialog() == 1)
			{
				if (ScheduleConflictsWith(frmTimeInput2.StartTime, frmTimeInput2.StopTime))
				{
					ShowScheduleConflictError();
				}
				else
				{
					lvwTimes.get_Items().Add(CreateLVI(frmTimeInput2.StartTime, frmTimeInput2.StopTime, frmTimeInput2.Comment));
					SelectItem(frmTimeInput2.StartTime);
				}
			}
			AddLableOfNumberOfEvents();
		}

		private void AddLableOfNumberOfEvents()
		{
			if (lvwTimes.get_Items().get_Count() < 1)
			{
				((Control)grpSchedule).set_Text("Schedule");
			}
			else if (lvwTimes.get_Items().get_Count() == 1)
			{
				((Control)grpSchedule).set_Text("Schedule of 1 event");
			}
			else
			{
				((Control)grpSchedule).set_Text("Schedule of " + lvwTimes.get_Items().get_Count() + " events");
			}
			for (int i = 0; i < lvwTimes.get_Items().get_Count(); i++)
			{
				lvwTimes.get_Items().get_Item(i).get_SubItems()
					.get_Item(3)
					.set_Text((i + 1).ToString());
			}
		}

		internal void AddAnEvent_External(DateTime StartTime, DateTime EndTime, string Descriptor, int Counter)
		{
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0033: Invalid comparison between Unknown and I4
			frmTimeInput frmTimeInput2 = new frmTimeInput("Add event #" + Counter);
			frmTimeInput2.StartTime = StartTime;
			frmTimeInput2.StopTime = EndTime;
			frmTimeInput2.Comment = Descriptor;
			if ((int)((Form)frmTimeInput2).ShowDialog() == 1)
			{
				if (ScheduleConflictsWith(frmTimeInput2.StartTime, frmTimeInput2.StopTime))
				{
					ShowScheduleConflictError();
				}
				else
				{
					lvwTimes.get_Items().Add(CreateLVI(frmTimeInput2.StartTime, frmTimeInput2.StopTime, frmTimeInput2.Comment));
					SelectItem(frmTimeInput2.StartTime);
				}
			}
			AddLableOfNumberOfEvents();
		}

		private void btnEdit_Click(object sender, EventArgs e)
		{
			//IL_0083: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Invalid comparison between Unknown and I4
			if (lvwTimes.get_SelectedItems().get_Count() != 1)
			{
				return;
			}
			ListViewItem val = lvwTimes.get_SelectedItems().get_Item(0);
			frmTimeInput frmTimeInput2 = new frmTimeInput("Edit Time");
			frmTimeInput2.StartTime = (DateTime)val.get_SubItems().get_Item(0).get_Tag();
			frmTimeInput2.StopTime = (DateTime)val.get_SubItems().get_Item(1).get_Tag();
			frmTimeInput2.Comment = val.get_SubItems().get_Item(2).get_Text();
			if ((int)((Form)frmTimeInput2).ShowDialog() == 1)
			{
				int num = lvwTimes.get_SelectedIndices().get_Item(0);
				if (ScheduleConflictsWith(frmTimeInput2.StartTime, frmTimeInput2.StopTime, num))
				{
					ShowScheduleConflictError();
					return;
				}
				lvwTimes.get_Items().set_Item(num, CreateLVI(frmTimeInput2.StartTime, frmTimeInput2.StopTime, frmTimeInput2.Comment));
				SelectItem(frmTimeInput2.StartTime);
			}
		}

		private void btnRemove_Click(object sender, EventArgs e)
		{
			while (lvwTimes.get_SelectedItems().get_Count() > 0)
			{
				lvwTimes.get_SelectedItems().get_Item(0).Remove();
			}
			AddLableOfNumberOfEvents();
		}

		private void cmdRemoveAllOld_Click(object sender, EventArgs e)
		{
			RemoveAllOld();
		}

		private void RemoveAllOld()
		{
			DateTime now = DateTime.Now;
			for (int i = 0; i < lvwTimes.get_Items().get_Count(); i++)
			{
				lvwTimes.get_Items().get_Item(i).set_Selected((DateTime)lvwTimes.get_Items().get_Item(i).get_SubItems()
					.get_Item(1)
					.get_Tag() < now);
			}
			while (lvwTimes.get_SelectedItems().get_Count() > 0)
			{
				lvwTimes.get_SelectedItems().get_Item(0).Remove();
			}
			AddLableOfNumberOfEvents();
		}

		private void btnClear_Click(object sender, EventArgs e)
		{
			lvwTimes.get_Items().Clear();
			AddLableOfNumberOfEvents();
		}

		private void chkEnableTimer_CheckedChanged(object sender, EventArgs e)
		{
			//IL_00e9: Unknown result type (might be due to invalid IL or missing references)
			bool @checked = chkEnableTimer.get_Checked();
			bool flag = _timerThread != null && _timerThread.IsAlive;
			((Form)this).set_TopMost(@checked);
			((Control)btnAdd).set_Enabled(!@checked);
			((Control)btnEdit).set_Enabled(!@checked);
			((Control)btnRemove).set_Enabled(!@checked);
			((Control)cmdRemoveAllOld).set_Enabled(!@checked);
			((Control)btnClear).set_Enabled(!@checked);
			((Control)lblCaptureWindowFoundDesc).set_Visible(@checked);
			((Control)lblCaptureWindowFound).set_Text(string.Empty);
			((Control)lblCaptureWindowFound).set_Visible(@checked);
			if (@checked)
			{
				if (!flag)
				{
					_timerEnabled = true;
					_timerThread = new Thread(RunTimer);
					_timerThread.Start();
				}
				else
				{
					chkEnableTimer.set_Checked(false);
					MessageBox.Show("Cannot start timer, the old thread is still running.", "Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
			}
			else
			{
				RemoveAllOld();
				if (flag)
				{
					_timerEnabled = false;
				}
			}
			((Control)lblEnabled).set_Visible(_timerEnabled);
			if (chkEnableTimer.get_Checked())
			{
				((Control)chkEnableTimer).set_Text("click to Disable Timer");
				((Control)chkEnableTimer).set_BackColor(Color.Cyan);
			}
			else
			{
				((Control)chkEnableTimer).set_Text("click to Enable Timer");
				((Control)chkEnableTimer).set_BackColor(Color.Yellow);
			}
		}

		private bool TimesOverlap(DateTime start1, DateTime stop1, DateTime start2, DateTime stop2)
		{
			if (!(stop2 <= start1.AddSeconds(-8.0)))
			{
				return !(start2 >= stop1.AddSeconds(8.0));
			}
			return false;
		}

		private bool ScheduleConflictsWith(DateTime startTime, DateTime stopTime, int ignoreIndex)
		{
			for (int i = 0; i < lvwTimes.get_Items().get_Count(); i++)
			{
				if (i != ignoreIndex)
				{
					ListViewItem val = lvwTimes.get_Items().get_Item(i);
					if (TimesOverlap((DateTime)val.get_SubItems().get_Item(0).get_Tag(), (DateTime)val.get_SubItems().get_Item(1).get_Tag(), startTime, stopTime))
					{
						return true;
					}
				}
			}
			return false;
		}

		private bool ScheduleConflictsWith(DateTime startTime, DateTime stopTime)
		{
			return ScheduleConflictsWith(startTime, stopTime, -1);
		}

		private void ShowScheduleConflictError()
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("The specified time range overlaps another time range in the schedule.\r\nMake sure there are at least " + 8 + " seconds between events.", "Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
		}

		private void SelectItem(DateTime startTime)
		{
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			//IL_002f: Expected O, but got Unknown
			lvwTimes.get_SelectedItems().Clear();
			foreach (ListViewItem item in lvwTimes.get_Items())
			{
				ListViewItem val = item;
				if ((DateTime)val.get_SubItems().get_Item(0).get_Tag() == startTime)
				{
					val.set_Selected(true);
					break;
				}
			}
		}

		private ListViewItem CreateLVI(DateTime startTime, DateTime stopTime, string comment)
		{
			//IL_0030: Unknown result type (might be due to invalid IL or missing references)
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0064: Expected O, but got Unknown
			ListViewItem val = new ListViewItem(new string[4]
			{
				startTime.ToString("yyyy-MM-dd  HH:mm:ss"),
				stopTime.ToString("yyyy-MM-dd  HH:mm:ss"),
				comment,
				""
			});
			val.get_SubItems().get_Item(0).set_Tag((object)startTime);
			val.get_SubItems().get_Item(1).set_Tag((object)stopTime);
			return val;
		}

		private void RunTimer()
		{
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0034: Expected O, but got Unknown
			int timeCount = 0;
			DateTime[] startTime = null;
			DateTime[] stopTime = null;
			int i;
			((Control)this).Invoke((Delegate)(MethodInvoker)delegate
			{
				timeCount = lvwTimes.get_Items().get_Count();
				startTime = new DateTime[timeCount];
				stopTime = new DateTime[timeCount];
				for (i = 0; i < timeCount; i++)
				{
					ListViewItem val = lvwTimes.get_Items().get_Item(i);
					startTime[i] = (DateTime)val.get_SubItems().get_Item(0).get_Tag();
					stopTime[i] = (DateTime)val.get_SubItems().get_Item(1).get_Tag();
				}
			});
			while (_timerEnabled)
			{
				int num = 0;
				if (FindCaptureWindow())
				{
					DateTime now = DateTime.Now;
					for (i = num; i < timeCount && !(now < startTime[i]); i++)
					{
						if (now > stopTime[i])
						{
							num = i + 1;
						}
						else
						{
							if (!(now >= startTime[i]) || !(now < stopTime[i]))
							{
								continue;
							}
							VDCapture.GetCapturePath();
							if (!VDCapture.IsCapturing())
							{
								VDCapture.StartCapture();
							}
							while (DateTime.Now < stopTime[i])
							{
								if (!_timerEnabled)
								{
									return;
								}
								Thread.Sleep(100);
								FindCaptureWindow();
							}
							VDCapture.StopCapture();
						}
					}
				}
				Thread.Sleep(100);
			}
		}

		private bool FindCaptureWindow()
		{
			//IL_004f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0059: Expected O, but got Unknown
			bool found = VDCapture.FindCaptureWindow();
			string Source = " VirtualDub ";
			if (VDCapture.UsingLiMovie)
			{
				Source = " Limovie ";
			}
			if (VDCapture.UsingGStar)
			{
				Source = "GStar-EX";
			}
			((Control)this).BeginInvoke((Delegate)(MethodInvoker)delegate
			{
				((Control)lblCaptureWindowFound).set_Text(found ? Source : " None ");
				if (found)
				{
					((Control)lblEnabled).set_Text("E N A B L E D");
					((Control)lblEnabled).set_Left(260);
					Label obj = lblEnabled;
					Color yellow;
					((Control)lblCaptureWindowFound).set_BackColor(yellow = Color.Yellow);
					((Control)obj).set_BackColor(yellow);
					Label obj2 = lblEnabled;
					((Control)lblCaptureWindowFound).set_ForeColor(yellow = Color.DarkRed);
					((Control)obj2).set_ForeColor(yellow);
				}
				else
				{
					((Control)lblEnabled).set_Text("No video capture window");
					((Control)lblEnabled).set_Left(210);
					Label obj3 = lblEnabled;
					Color yellow;
					((Control)lblCaptureWindowFound).set_BackColor(yellow = Color.Red);
					((Control)obj3).set_BackColor(yellow);
					Label obj4 = lblEnabled;
					((Control)lblCaptureWindowFound).set_ForeColor(yellow = Color.Yellow);
					((Control)obj4).set_ForeColor(yellow);
				}
			});
			return found;
		}

		private string GetMyAppDataDir()
		{
			string text = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "VDTimer");
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			return text;
		}

		private void LoadSchedule()
		{
			try
			{
				StreamReader streamReader;
				using (streamReader = new StreamReader(TimerSchedule, Encoding.UTF8))
				{
					string text;
					while ((text = streamReader.ReadLine()) != null)
					{
						string[] array = text.Split(new char[1] { ',' }, 3);
						DateTime startTime = new DateTime(long.Parse(array[0]));
						DateTime stopTime = new DateTime(long.Parse(array[1]));
						string comment = array[2];
						if (!ScheduleConflictsWith(startTime, stopTime))
						{
							lvwTimes.get_Items().Add(CreateLVI(startTime, stopTime, comment));
						}
					}
				}
			}
			catch
			{
			}
			RemoveAllOld();
		}

		private void SaveSchedule()
		{
			//IL_0030: Unknown result type (might be due to invalid IL or missing references)
			//IL_0035: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				StreamWriter streamWriter;
				using (streamWriter = new StreamWriter(TimerSchedule, append: false, Encoding.UTF8))
				{
					foreach (ListViewItem item in lvwTimes.get_Items())
					{
						DateTime dateTime = (DateTime)item.get_SubItems().get_Item(0).get_Tag();
						DateTime dateTime2 = (DateTime)item.get_SubItems().get_Item(1).get_Tag();
						string text = item.get_SubItems().get_Item(2).get_Text();
						string value = $"{dateTime.Ticks},{dateTime2.Ticks},{text}";
						streamWriter.WriteLine(value);
					}
				}
			}
			catch
			{
			}
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"VirtualDub Timer");
		}

		private void FrmVDTimer_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(634);
			if (((Control)this).get_Height() < 300)
			{
				((Control)this).set_Height(300);
			}
			((Control)grpSchedule).set_Height(((Control)this).get_Height() - 84);
			((Control)lvwTimes).set_Height(((Control)grpSchedule).get_Height() - 54);
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
			//IL_0c2e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c38: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(frmVDTimer));
			grpSchedule = new GroupBox();
			cmdHelp = new Button();
			cmdRemoveAllOld = new Button();
			btnClear = new Button();
			btnEdit = new Button();
			btnRemove = new Button();
			btnAdd = new Button();
			lvwTimes = new ListView();
			chStartTime = new ColumnHeader();
			chStopTime = new ColumnHeader();
			chComment = new ColumnHeader();
			chCount = new ColumnHeader();
			chkEnableTimer = new CheckBox();
			lblCaptureWindowFoundDesc = new Label();
			lblCaptureWindowFound = new Label();
			label1 = new Label();
			lblEnabled = new Label();
			label2 = new Label();
			((Control)grpSchedule).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpSchedule).get_Controls().Add((Control)(object)cmdHelp);
			((Control)grpSchedule).get_Controls().Add((Control)(object)cmdRemoveAllOld);
			((Control)grpSchedule).get_Controls().Add((Control)(object)btnClear);
			((Control)grpSchedule).get_Controls().Add((Control)(object)btnEdit);
			((Control)grpSchedule).get_Controls().Add((Control)(object)btnRemove);
			((Control)grpSchedule).get_Controls().Add((Control)(object)btnAdd);
			((Control)grpSchedule).get_Controls().Add((Control)(object)lvwTimes);
			((Control)grpSchedule).set_Location(new Point(8, 4));
			((Control)grpSchedule).set_Name("grpSchedule");
			((Control)grpSchedule).set_Size(new Size(602, 229));
			((Control)grpSchedule).set_TabIndex(0);
			grpSchedule.set_TabStop(false);
			((Control)grpSchedule).set_Text("Schedule");
			((Control)cmdHelp).set_Anchor((AnchorStyles)6);
			((Control)cmdHelp).set_BackgroundImageLayout((ImageLayout)0);
			((Control)cmdHelp).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdHelp).set_Location(new Point(538, 200));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(52, 23));
			((Control)cmdHelp).set_TabIndex(6);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)cmdRemoveAllOld).set_Anchor((AnchorStyles)6);
			((Control)cmdRemoveAllOld).set_Location(new Point(325, 200));
			((Control)cmdRemoveAllOld).set_Name("cmdRemoveAllOld");
			((Control)cmdRemoveAllOld).set_Size(new Size(85, 23));
			((Control)cmdRemoveAllOld).set_TabIndex(4);
			((Control)cmdRemoveAllOld).set_Text("Remove all old");
			((ButtonBase)cmdRemoveAllOld).set_UseVisualStyleBackColor(true);
			((Control)cmdRemoveAllOld).add_Click((EventHandler)cmdRemoveAllOld_Click);
			((Control)btnClear).set_Anchor((AnchorStyles)6);
			((Control)btnClear).set_Location(new Point(446, 200));
			((Control)btnClear).set_Name("btnClear");
			((Control)btnClear).set_Size(new Size(56, 23));
			((Control)btnClear).set_TabIndex(5);
			((Control)btnClear).set_Text("Clear all");
			((ButtonBase)btnClear).set_UseVisualStyleBackColor(true);
			((Control)btnClear).add_Click((EventHandler)btnClear_Click);
			((Control)btnEdit).set_Anchor((AnchorStyles)6);
			((Control)btnEdit).set_Location(new Point(98, 200));
			((Control)btnEdit).set_Name("btnEdit");
			((Control)btnEdit).set_Size(new Size(52, 23));
			((Control)btnEdit).set_TabIndex(2);
			((Control)btnEdit).set_Text("Edit");
			((ButtonBase)btnEdit).set_UseVisualStyleBackColor(true);
			((Control)btnEdit).add_Click((EventHandler)btnEdit_Click);
			((Control)btnRemove).set_Anchor((AnchorStyles)6);
			((Control)btnRemove).set_Location(new Point(186, 200));
			((Control)btnRemove).set_Name("btnRemove");
			((Control)btnRemove).set_Size(new Size(103, 23));
			((Control)btnRemove).set_TabIndex(3);
			((Control)btnRemove).set_Text("Remove selected");
			((ButtonBase)btnRemove).set_UseVisualStyleBackColor(true);
			((Control)btnRemove).add_Click((EventHandler)btnRemove_Click);
			((Control)btnAdd).set_Anchor((AnchorStyles)6);
			((Control)btnAdd).set_Location(new Point(10, 200));
			((Control)btnAdd).set_Name("btnAdd");
			((Control)btnAdd).set_Size(new Size(52, 23));
			((Control)btnAdd).set_TabIndex(1);
			((Control)btnAdd).set_Text("Add");
			((ButtonBase)btnAdd).set_UseVisualStyleBackColor(true);
			((Control)btnAdd).add_Click((EventHandler)btnAdd_Click);
			lvwTimes.get_Columns().AddRange((ColumnHeader[])(object)new ColumnHeader[4] { chStartTime, chStopTime, chComment, chCount });
			((Control)lvwTimes).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			lvwTimes.set_FullRowSelect(true);
			lvwTimes.set_GridLines(true);
			lvwTimes.set_HeaderStyle((ColumnHeaderStyle)1);
			lvwTimes.set_HideSelection(false);
			lvwTimes.set_LabelWrap(false);
			((Control)lvwTimes).set_Location(new Point(10, 20));
			((Control)lvwTimes).set_Name("lvwTimes");
			((Control)lvwTimes).set_Size(new Size(583, 175));
			lvwTimes.set_Sorting((SortOrder)1);
			((Control)lvwTimes).set_TabIndex(0);
			lvwTimes.set_UseCompatibleStateImageBehavior(false);
			lvwTimes.set_View((View)1);
			chStartTime.set_DisplayIndex(1);
			chStartTime.set_Text("Start Time");
			chStartTime.set_Width(160);
			chStopTime.set_DisplayIndex(2);
			chStopTime.set_Text("Stop Time");
			chStopTime.set_Width(160);
			chComment.set_DisplayIndex(3);
			chComment.set_Text("Event identification");
			chComment.set_Width(200);
			chCount.set_DisplayIndex(0);
			chCount.set_Text("#");
			chCount.set_TextAlign((HorizontalAlignment)2);
			chCount.set_Width(40);
			((Control)chkEnableTimer).set_Anchor((AnchorStyles)6);
			chkEnableTimer.set_Appearance((Appearance)1);
			((Control)chkEnableTimer).set_AutoSize(true);
			((Control)chkEnableTimer).set_BackColor(Color.Yellow);
			((ButtonBase)chkEnableTimer).get_FlatAppearance().set_BorderColor(Color.Black);
			((ButtonBase)chkEnableTimer).get_FlatAppearance().set_CheckedBackColor(SystemColors.Control);
			((Control)chkEnableTimer).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkEnableTimer).set_Location(new Point(106, 237));
			((Control)chkEnableTimer).set_Name("chkEnableTimer");
			((Control)chkEnableTimer).set_Size(new Size(133, 23));
			((Control)chkEnableTimer).set_TabIndex(1);
			((Control)chkEnableTimer).set_Text("click to Enable Timer");
			((ButtonBase)chkEnableTimer).set_UseVisualStyleBackColor(false);
			chkEnableTimer.add_CheckedChanged((EventHandler)chkEnableTimer_CheckedChanged);
			((Control)lblCaptureWindowFoundDesc).set_Anchor((AnchorStyles)6);
			((Control)lblCaptureWindowFoundDesc).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCaptureWindowFoundDesc).set_Location(new Point(326, 242));
			((Control)lblCaptureWindowFoundDesc).set_Name("lblCaptureWindowFoundDesc");
			((Control)lblCaptureWindowFoundDesc).set_Size(new Size(174, 18));
			((Control)lblCaptureWindowFoundDesc).set_TabIndex(2);
			((Control)lblCaptureWindowFoundDesc).set_Text("Video Capture Window Found:");
			((Control)lblCaptureWindowFoundDesc).set_Visible(false);
			((Control)lblCaptureWindowFound).set_Anchor((AnchorStyles)6);
			((Control)lblCaptureWindowFound).set_AutoSize(true);
			lblCaptureWindowFound.set_BorderStyle((BorderStyle)1);
			((Control)lblCaptureWindowFound).set_Font(new Font("Tahoma", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCaptureWindowFound).set_Location(new Point(502, 240));
			((Control)lblCaptureWindowFound).set_Name("lblCaptureWindowFound");
			((Control)lblCaptureWindowFound).set_Size(new Size(22, 19));
			((Control)lblCaptureWindowFound).set_TabIndex(3);
			((Control)lblCaptureWindowFound).set_Text("...");
			((Control)lblCaptureWindowFound).set_Visible(false);
			((Control)label1).set_Anchor((AnchorStyles)6);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(-1, 263));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(602, 12));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Based on VDTimer  © 2005-2007  Moitah (moitah@yahoo.com).   Incorporated into Occult && adapted to use Limovie, GStar, or VirtualDub - 2015/16");
			((Control)lblEnabled).set_AutoSize(true);
			((Control)lblEnabled).set_BackColor(Color.Yellow);
			lblEnabled.set_BorderStyle((BorderStyle)1);
			((Control)lblEnabled).set_Font(new Font("Tahoma", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEnabled).set_ForeColor(Color.DarkRed);
			((Control)lblEnabled).set_Location(new Point(260, 2));
			((Control)lblEnabled).set_Name("lblEnabled");
			((Control)lblEnabled).set_Size(new Size(101, 19));
			((Control)lblEnabled).set_TabIndex(5);
			((Control)lblEnabled).set_Text("E N A B L E D");
			((Control)lblEnabled).set_Visible(false);
			((Control)label2).set_Anchor((AnchorStyles)6);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_BackColor(Color.Honeydew);
			((Control)label2).set_Font(new Font("Tahoma", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_ForeColor(Color.Maroon);
			((Control)label2).set_Location(new Point(8, 235));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(75, 24));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Allow 8 secs \r\nbetween events");
			label2.set_TextAlign(ContentAlignment.TopCenter);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(618, 274));
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)lblEnabled);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lblCaptureWindowFound);
			((Control)this).get_Controls().Add((Control)(object)lblCaptureWindowFoundDesc);
			((Control)this).get_Controls().Add((Control)(object)chkEnableTimer);
			((Control)this).get_Controls().Add((Control)(object)grpSchedule);
			((Control)this).set_Font(new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("frmVDTimer");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Recording Timer - for Limovie, GStar, and VirtualDub");
			((Form)this).add_FormClosed(new FormClosedEventHandler(frmVDTimer_FormClosed));
			((Form)this).add_Load((EventHandler)frmVDTimer_Load);
			((Control)this).add_Resize((EventHandler)FrmVDTimer_Resize);
			((Control)grpSchedule).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
