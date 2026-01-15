using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult;
using Occult.Properties;

namespace AOTA
{
	public class AOTA_SetTimeScale : Form
	{
		private int GridTimesSelectedRow;

		private bool AddingRows;

		private bool HaveFilled;

		private bool HaveSet;

		private IContainer components;

		private Label label46;

		private Button cmdSetTime;

		private Label label44;

		private Label label43;

		private Label label42;

		private Label label7;

		private TextBox txtFrameID;

		private TextBox txtMins;

		private TextBox txtSecs;

		private TextBox txtHrs;

		internal DataGridView gridFrameTimes;

		private DataGridViewTextBoxColumn Frame;

		private DataGridViewTextBoxColumn UTC;

		private NumericUpDown updnFrameRate;

		private Label label47;

		private Label label45;

		private Button cmdSetTimeUser;

		private Button cmdSetTimePAL;

		private Button cmdSetTimeNTSC;

		private Label label1;

		private Label label2;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdExit;

		private GroupBox groupBox1;

		private TextBox txtExplan;

		private ComboBox cmbVTI;

		private Label label3;

		private ToolStripMenuItem vTIInfoLinksToolStripMenuItem;

		private Label lblTangraAdvice;

		private Label label4;

		private CheckBox chkSecDecPlaces;

		public AOTA_SetTimeScale()
		{
			InitializeComponent();
		}

		private void AOTA_SetTimeScale_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			gridFrameTimes.get_Rows().Clear();
			AddingRows = true;
			int decimalLength = 3;
			if (chkSecDecPlaces.get_Checked())
			{
				decimalLength = 4;
			}
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				gridFrameTimes.get_Rows().Add(new object[2]
				{
					AOTAData.FrameID[i].ToString(),
					Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[i] / 3600.0, 2, decimalLength, MinutesOnly: false)
				});
			}
			AddingRows = false;
			((ListControl)cmbVTI).set_SelectedIndex(Settings.Default.AOTA_VTI);
			if ((AOTAData.FrameTimeSecs[0] != 0.0) & (AOTAData.FrameTimeSecs[AOTAData.StarCount - 1] > AOTAData.FrameTimeSecs[0]) & AOTAData.TwoAndOnlyTwoTimesPresentInSourceData)
			{
				double num = AOTAData.FrameID[1] - AOTAData.FrameID[0];
				if (num < 0.2)
				{
					num = 1.0;
				}
				updnFrameRate.set_Value((decimal)((double)(AOTAData.StarCount - 1) / (AOTAData.FrameTimeSecs[AOTAData.StarCount - 1] - AOTAData.FrameTimeSecs[0]) * num));
			}
			else if ((AOTAData.FrameTimeSecs[0] == 0.0) | (AOTAData.FrameTimeSecs[1] == 0.0))
			{
				int num2 = -1;
				int num3 = -1;
				int num4 = -1;
				for (int j = 0; j < AOTAData.StarCount; j++)
				{
					if (AOTAData.FrameTimeSecs[j] != 0.0)
					{
						num2 = j;
						break;
					}
				}
				if (num2 >= 0)
				{
					for (int num5 = AOTAData.StarCount - 1; num5 >= 0; num5--)
					{
						if (AOTAData.FrameTimeSecs[num5] != 0.0)
						{
							num3 = num5;
							num4 = num3 - num2;
							break;
						}
					}
				}
				if (num2 >= 0 && num3 >= 0 && ((num4 > 0) & (AOTAData.FrameTimeSecs[num3] > AOTAData.FrameTimeSecs[num2])))
				{
					updnFrameRate.set_Value((decimal)((double)num4 / (AOTAData.FrameTimeSecs[num3] - AOTAData.FrameTimeSecs[num2])));
					gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(num2).get_Cells()
						.get_Item(0));
					GridTimesSelectedRow = num2;
					SetTimeForSelectedRow();
					((Control)lblTangraAdvice).set_Visible(true);
				}
			}
			((Control)this).set_Text("Set or adjust the time scale manually : ");
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				((Control)this).set_Text(((Control)this).get_Text() + AOTAData.TangraSourceFile);
			}
			else
			{
				((Control)this).set_Text(((Control)this).get_Text() + AOTAData.CurrentFileName);
			}
		}

		private void gridFrameTimes_RowEnter(object sender, DataGridViewCellEventArgs e)
		{
			GridTimesSelectedRow = e.get_RowIndex();
			if (!AddingRows)
			{
				SetTimeForSelectedRow();
			}
		}

		private void SetTimeForSelectedRow()
		{
			((Control)txtFrameID).set_Text(gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(0)
				.get_Value()
				.ToString());
			string text = gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(1)
				.get_Value()
				.ToString();
			if (text.PadRight(2).Substring(0, 2).Trim() == "0")
			{
				TextBox obj = txtHrs;
				TextBox obj2 = txtMins;
				string text2;
				((Control)txtSecs).set_Text(text2 = "0");
				string text3;
				((Control)obj2).set_Text(text3 = text2);
				((Control)obj).set_Text(text3);
			}
			else
			{
				((Control)txtHrs).set_Text(text.Substring(0, 2));
				((Control)txtMins).set_Text(text.Substring(3, 2));
				((Control)txtSecs).set_Text(text.Substring(6));
			}
			if (!int.TryParse(gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(0)
				.get_Value()
				.ToString(), out var result))
			{
				result = 0;
			}
			HaveSet = true;
			Display(result);
			if (GridTimesSelectedRow > 0 && text.PadRight(2).Substring(0, 2).Trim() == "0")
			{
				((Control)txtHrs).Focus();
			}
		}

		private void Display(int Frame)
		{
			if (AOTA_ExternalAccess.AOTA_Client != null)
			{
				AOTA_ExternalAccess.FrameID_DisplayedInTangra = Frame;
				AOTA_ExternalAccess.AOTA_Client.PositionToFrame(Frame);
				AOTA_ExternalAccess.TangraFrameDisplayFromAOTA = true;
				AOTA_ExternalAccess.TangraFrameDisplayFromTangra = false;
			}
		}

		private void cmdSetTime_Click(object sender, EventArgs e)
		{
			int decimalLength = 3;
			if (chkSecDecPlaces.get_Checked())
			{
				decimalLength = 4;
			}
			if (!int.TryParse(((Control)txtHrs).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtMins).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtSecs).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			AOTAData.FrameTimeSecs[GridTimesSelectedRow] = (double)(result * 3600 + result2 * 60) + result3;
			gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(1)
				.set_Value((object)Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[GridTimesSelectedRow] / 3600.0, 2, decimalLength, MinutesOnly: false));
		}

		private void cmdSetTimeNTSC_Click(object sender, EventArgs e)
		{
			updnFrameRate.set_Value(29.97m);
			FillTimes(29.97);
		}

		private void cmdSetTimePAL_Click(object sender, EventArgs e)
		{
			updnFrameRate.set_Value(25m);
			FillTimes(25.0);
		}

		private void cmdSetTimeUser_Click(object sender, EventArgs e)
		{
			FillTimes((double)updnFrameRate.get_Value());
		}

		private void FillTimes(double FrameRate)
		{
			int decimalLength = 3;
			if (chkSecDecPlaces.get_Checked())
			{
				decimalLength = 4;
			}
			double num = 1.0 / FrameRate;
			if ((AOTAData.FrameID[0] % 1f > 0f) | (AOTAData.FrameID[1] % 1f > 0f))
			{
				num /= 2.0;
			}
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				if (i != GridTimesSelectedRow)
				{
					AOTAData.FrameTimeSecs[i] = AOTAData.FrameTimeSecs[GridTimesSelectedRow] + (double)(i - GridTimesSelectedRow) * num;
					gridFrameTimes.get_Rows().get_Item(i).get_Cells()
						.get_Item(1)
						.set_Value((object)Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[i] / 3600.0, 2, decimalLength, MinutesOnly: false));
				}
			}
			HaveFilled = true;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			MyClose();
		}

		private void AOTA_SetTimeScale_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			MyClose();
		}

		private void MyClose()
		{
			//IL_0066: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Invalid comparison between Unknown and I4
			if (!HaveSet | !HaveFilled)
			{
				string text = "";
				text = ((!HaveSet & !HaveFilled) ? "You have not set a time, nor filled the table." : (HaveSet ? "You have not filled the table using the indicated frame rate." : "You have not set a time."));
				if ((int)MessageBox.Show(text + "\r\n\r\nAre you sure you want to leave this form?", "Unfinished entries", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256) == 7)
				{
					return;
				}
			}
			((Form)this).Close();
		}

		private void SetText(int x)
		{
			string text = "The time stamp from a VTI relates to when a video frame or field is output from the camera. \r\n\r\nAOTA needs to know the START time of the camera OUTPUT (not exposure) to correctly determine the event time. Corrections for delays occuring within the camera are applied in Tab 5 ('Camera corrections and final times'). \r\n\r\n";
			switch (x)
			{
			case 0:
				text += "The IOTA-VTI inserts two time stamps on each video frame - one for each field that is received in the VTI.\r\n\r\nFor videos measured at frame intervals, select the earlier time stamp to get the start time for the camera output of the video frame.\r\n\r\nFor videos measured at field intervals, each field has a single time stamp which refers to the start time for the camera output of that field.";
				break;
			case 1:
				text += "The KIWI-OSD displays two time stamps on each video field.\r\n\r\nFor videos measured at frame intervals, one time stamp will appear blurred, while the other time stamp is clear. Select the clear time stamp to get the start time for the camera output of the video frame.\r\n\r\nFor videos measured at field intervals, each field has two time stamps. Select the LATER value to get the start time for the camera output of each field.";
				break;
			case 2:
				text += "The TIM-10 (Cuno-VTI) inserts two time stamps on each video field.\r\n\r\nFor videos measured at frame intervals, one time stamp will appear blurred, while the other time stamp is clear. Select the clear time stamp, and ADD either 0.020 secs (PAL) or 0.017 secs (NTSC) to get the start time for the camera output of the video frame.\r\n\r\nFor videos measured at field intervals, each field has two time stamps. Select the LATER value and ADD either 0.020 secs (PAL) or 0.017 secs (NTSC) to get the start time for the camera output of each field.";
				break;
			case 3:
				text += "The Anderson-VTI (Svensoft) inserts two time stamps on each video frame - one for each field that is received in the VTI.\r\n\r\nFor videos measured at frame intervals, select the earlier time stamp to get the start time for the camera output of the video frame.\r\n\r\nFor videos measured at field intervals, each field has a single time stamp which refers to the start time for the camera output of that field.";
				break;
			case 4:
				text += "The characteristics of the GHS-OSD are not available in AOTA. However it is believed the time stamp refers to the start time for the camera output of that frame.";
				break;
			case 5:
				text += "The GPSBoxSprite2 displays two time stamps on each video field.\r\n\r\nFor videos measured at frame intervals, one time stamp will appear blurred, while the other time stamp is clear. Select the clear time stamp to get the start time for the camera output of the video frame.\r\n\r\nFor videos measured at field intervals, each field has two time stamps. Select the LATER value to get the start time for the camera output of each field.";
				break;
			case 6:
				text += "The times in an ADVS CSV file prepared using Tangra refer to the mid-time of the corresponding measurement from Tangra. The times given in the list at the left of this form differ from those in the CSV file, as they have been adjusted to refer to the time at the Start of the corresponding measurement.";
				break;
			}
			((Control)txtExplan).set_Text(text);
		}

		private void cmbVTI_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.AOTA_VTI = ((ListControl)cmbVTI).get_SelectedIndex();
			SetText(((ListControl)cmbVTI).get_SelectedIndex());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Set time scale");
		}

		private void vTIInfoLinksToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"VTI links");
		}

		private void txtHrs_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHrs).SelectAll();
		}

		private void txtMins_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMins).SelectAll();
		}

		private void txtSecs_MouseEnter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSecs).SelectAll();
		}

		private void chkSecDecPlaces_CheckedChanged(object sender, EventArgs e)
		{
			int num = 3;
			if (chkSecDecPlaces.get_Checked())
			{
				num = 4;
			}
			updnFrameRate.set_DecimalPlaces(num);
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				gridFrameTimes.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.set_Value((object)Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[i] / 3600.0, 2, num, MinutesOnly: false));
			}
			SetTimeForSelectedRow();
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
			//IL_06b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_06bc: Expected O, but got Unknown
			//IL_120a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1214: Expected O, but got Unknown
			//IL_14d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_14de: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AOTA_SetTimeScale));
			label46 = new Label();
			cmdSetTime = new Button();
			label44 = new Label();
			label43 = new Label();
			label42 = new Label();
			label7 = new Label();
			txtFrameID = new TextBox();
			txtMins = new TextBox();
			txtSecs = new TextBox();
			txtHrs = new TextBox();
			gridFrameTimes = new DataGridView();
			Frame = new DataGridViewTextBoxColumn();
			UTC = new DataGridViewTextBoxColumn();
			updnFrameRate = new NumericUpDown();
			label47 = new Label();
			label45 = new Label();
			cmdSetTimeUser = new Button();
			cmdSetTimePAL = new Button();
			cmdSetTimeNTSC = new Button();
			label1 = new Label();
			label2 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			vTIInfoLinksToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdExit = new Button();
			groupBox1 = new GroupBox();
			cmbVTI = new ComboBox();
			label3 = new Label();
			txtExplan = new TextBox();
			lblTangraAdvice = new Label();
			label4 = new Label();
			chkSecDecPlaces = new CheckBox();
			((ISupportInitialize)gridFrameTimes).BeginInit();
			((ISupportInitialize)updnFrameRate).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_ForeColor(Color.Navy);
			((Control)label46).set_Location(new Point(287, 54));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(204, 68));
			((Control)label46).set_TabIndex(18);
			((Control)label46).set_Text("For the selected frame:\r\nset the time at the START\r\nof the output of that frame \r\nfrom the camera \r\n");
			((Control)cmdSetTime).set_Location(new Point(289, 192));
			((Control)cmdSetTime).set_Name("cmdSetTime");
			((Control)cmdSetTime).set_Size(new Size(187, 34));
			((Control)cmdSetTime).set_TabIndex(9);
			((Control)cmdSetTime).set_Text("Set this time for the selected frame");
			((ButtonBase)cmdSetTime).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTime).add_Click((EventHandler)cmdSetTime_Click);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(291, 151));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(50, 13));
			((Control)label44).set_TabIndex(1);
			((Control)label44).set_Text("Frame ID");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(438, 151));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(26, 13));
			((Control)label43).set_TabIndex(7);
			((Control)label43).set_Text("Sec");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(394, 151));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(24, 13));
			((Control)label42).set_TabIndex(5);
			((Control)label42).set_Text("Min");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(363, 151));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(18, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Hr");
			((Control)txtFrameID).set_Location(new Point(289, 166));
			((Control)txtFrameID).set_Name("txtFrameID");
			((TextBoxBase)txtFrameID).set_ReadOnly(true);
			((Control)txtFrameID).set_Size(new Size(55, 20));
			((Control)txtFrameID).set_TabIndex(2);
			((Control)txtMins).set_Location(new Point(393, 166));
			((Control)txtMins).set_Name("txtMins");
			((Control)txtMins).set_Size(new Size(26, 20));
			((Control)txtMins).set_TabIndex(6);
			((Control)txtMins).add_Enter((EventHandler)txtMins_Enter);
			((Control)txtSecs).set_Location(new Point(427, 166));
			((Control)txtSecs).set_Name("txtSecs");
			((Control)txtSecs).set_Size(new Size(49, 20));
			((Control)txtSecs).set_TabIndex(8);
			((Control)txtSecs).add_MouseEnter((EventHandler)txtSecs_MouseEnter);
			((Control)txtHrs).set_Location(new Point(359, 166));
			((Control)txtHrs).set_Name("txtHrs");
			((Control)txtHrs).set_Size(new Size(26, 20));
			((Control)txtHrs).set_TabIndex(4);
			((Control)txtHrs).add_Enter((EventHandler)txtHrs_Enter);
			gridFrameTimes.set_AllowUserToAddRows(false);
			gridFrameTimes.set_AllowUserToDeleteRows(false);
			gridFrameTimes.set_AllowUserToResizeColumns(false);
			gridFrameTimes.set_AllowUserToResizeRows(false);
			gridFrameTimes.set_ColumnHeadersHeightSizeMode((DataGridViewColumnHeadersHeightSizeMode)2);
			gridFrameTimes.get_Columns().AddRange((DataGridViewColumn[])(object)new DataGridViewColumn[2]
			{
				(DataGridViewColumn)Frame,
				(DataGridViewColumn)UTC
			});
			((Control)gridFrameTimes).set_Location(new Point(38, 52));
			gridFrameTimes.set_MultiSelect(false);
			((Control)gridFrameTimes).set_Name("gridFrameTimes");
			gridFrameTimes.set_ReadOnly(true);
			gridFrameTimes.set_ScrollBars((ScrollBars)2);
			((Control)gridFrameTimes).set_Size(new Size(215, 354));
			((Control)gridFrameTimes).set_TabIndex(0);
			gridFrameTimes.add_RowEnter(new DataGridViewCellEventHandler(gridFrameTimes_RowEnter));
			((DataGridViewColumn)Frame).set_HeaderText("Frame #");
			((DataGridViewColumn)Frame).set_Name("Frame");
			((DataGridViewBand)Frame).set_ReadOnly(true);
			Frame.set_SortMode((DataGridViewColumnSortMode)0);
			((DataGridViewColumn)Frame).set_Width(50);
			((DataGridViewColumn)UTC).set_HeaderText("UTC at start of frame");
			((DataGridViewColumn)UTC).set_Name("UTC");
			((DataGridViewBand)UTC).set_ReadOnly(true);
			UTC.set_SortMode((DataGridViewColumnSortMode)0);
			((DataGridViewColumn)UTC).set_Width(120);
			updnFrameRate.set_DecimalPlaces(3);
			updnFrameRate.set_Increment(new decimal(new int[4] { 1, 0, 0, 196608 }));
			((Control)updnFrameRate).set_Location(new Point(337, 338));
			updnFrameRate.set_Maximum(new decimal(new int[4] { 600, 0, 0, 0 }));
			updnFrameRate.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnFrameRate).set_Name("updnFrameRate");
			((Control)updnFrameRate).set_Size(new Size(62, 20));
			((Control)updnFrameRate).set_TabIndex(15);
			updnFrameRate.set_Value(new decimal(new int[4] { 25, 0, 0, 0 }));
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(423, 282));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(72, 39));
			((Control)label47).set_TabIndex(16);
			((Control)label47).set_Text("even if Fields \r\nhave been \r\nmeasured...");
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(336, 322));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(67, 13));
			((Control)label45).set_TabIndex(14);
			((Control)label45).set_Text("Frame rate");
			label45.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdSetTimeUser).set_Location(new Point(325, 280));
			((Control)cmdSetTimeUser).set_Name("cmdSetTimeUser");
			((Control)cmdSetTimeUser).set_Size(new Size(88, 41));
			((Control)cmdSetTimeUser).set_TabIndex(12);
			((Control)cmdSetTimeUser).set_Text("Fill table using \r\nthis frame rate");
			((ButtonBase)cmdSetTimeUser).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTimeUser).add_Click((EventHandler)cmdSetTimeUser_Click);
			((Control)cmdSetTimePAL).set_Location(new Point(273, 232));
			((Control)cmdSetTimePAL).set_Name("cmdSetTimePAL");
			((Control)cmdSetTimePAL).set_Size(new Size(104, 41));
			((Control)cmdSetTimePAL).set_TabIndex(10);
			((Control)cmdSetTimePAL).set_Text("Fill table using the \r\nPAL frame rate");
			((ButtonBase)cmdSetTimePAL).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTimePAL).add_Click((EventHandler)cmdSetTimePAL_Click);
			((Control)cmdSetTimeNTSC).set_Location(new Point(387, 232));
			((Control)cmdSetTimeNTSC).set_Name("cmdSetTimeNTSC");
			((Control)cmdSetTimeNTSC).set_Size(new Size(104, 41));
			((Control)cmdSetTimeNTSC).set_TabIndex(11);
			((Control)cmdSetTimeNTSC).set_Text("Fill table using the\r\nNTSC frame rate");
			((ButtonBase)cmdSetTimeNTSC).set_UseVisualStyleBackColor(true);
			((Control)cmdSetTimeNTSC).add_Click((EventHandler)cmdSetTimeNTSC_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_ForeColor(Color.DarkRed);
			((Control)label1).set_Location(new Point(33, 28));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(444, 20));
			((Control)label1).set_TabIndex(19);
			((Control)label1).set_Text("NOTE: Changes made on this form cannot be 'undone'");
			((Control)label2).set_AutoSize(true);
			label2.set_BorderStyle((BorderStyle)1);
			((Control)label2).set_ForeColor(Color.MidnightBlue);
			((Control)label2).set_Location(new Point(12, 417));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(270, 67));
			((Control)label2).set_TabIndex(13);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)vTIInfoLinksToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(750, 24));
			((Control)menuStrip1).set_TabIndex(20);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("helpToolStripMenuItem.Image"));
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)vTIInfoLinksToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("vTIInfoLinksToolStripMenuItem.Image"));
			((ToolStripItem)vTIInfoLinksToolStripMenuItem).set_Name("vTIInfoLinksToolStripMenuItem");
			((ToolStripItem)vTIInfoLinksToolStripMenuItem).set_Size(new Size(122, 20));
			((ToolStripItem)vTIInfoLinksToolStripMenuItem).set_Text("VTI - info links    ");
			((ToolStripItem)vTIInfoLinksToolStripMenuItem).add_Click((EventHandler)vTIInfoLinksToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("exitToolStripMenuItem.Image"));
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(66, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit    ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdExit).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExit).set_Location(new Point(347, 440));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(111, 41));
			((Control)cmdExit).set_TabIndex(17);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbVTI);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtExplan);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(504, 30));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(234, 453));
			((Control)groupBox1).set_TabIndex(21);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Reading the VTI time stamp");
			((Control)cmbVTI).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbVTI).set_FormattingEnabled(true);
			cmbVTI.get_Items().AddRange(new object[7] { "IOTA - VTI", "KIWI - OSD", "TIM-10 (previously Cuno VTI )", "Sven Anderson VTI", "GHS - OSD", "GPSBoxSprite2", "ADVS files from TANGRA" });
			((Control)cmbVTI).set_Location(new Point(25, 40));
			((Control)cmbVTI).set_Name("cmbVTI");
			((Control)cmbVTI).set_Size(new Size(185, 24));
			((Control)cmbVTI).set_TabIndex(2);
			cmbVTI.add_SelectedIndexChanged((EventHandler)cmbVTI_SelectedIndexChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(33, 24));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(169, 15));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("Select your VTI for instructions");
			((Control)txtExplan).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtExplan).set_Location(new Point(5, 77));
			((TextBoxBase)txtExplan).set_Multiline(true);
			((Control)txtExplan).set_Name("txtExplan");
			((Control)txtExplan).set_Size(new Size(226, 369));
			((Control)txtExplan).set_TabIndex(1);
			((Control)lblTangraAdvice).set_AutoSize(true);
			((Control)lblTangraAdvice).set_BackColor(Color.Yellow);
			lblTangraAdvice.set_BorderStyle((BorderStyle)1);
			((Control)lblTangraAdvice).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblTangraAdvice).set_ForeColor(Color.Navy);
			((Control)lblTangraAdvice).set_Location(new Point(307, 368));
			((Control)lblTangraAdvice).set_Name("lblTangraAdvice");
			((Control)lblTangraAdvice).set_Size(new Size(163, 67));
			((Control)lblTangraAdvice).set_TabIndex(22);
			((Control)lblTangraAdvice).set_Text("This frame rate has been\r\ncalculated using two times \r\nread from Tangra. If OK, \r\nClick 'Fill table using this \r\nframe rate'.");
			((Control)lblTangraAdvice).set_Visible(false);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(402, 341));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(60, 13));
			((Control)label4).set_TabIndex(23);
			((Control)label4).set_Text("frames/sec");
			((Control)chkSecDecPlaces).set_AutoSize(true);
			chkSecDecPlaces.set_Checked(Settings.Default.AOTA_Integrity_DecPlaces);
			((Control)chkSecDecPlaces).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_Integrity_DecPlaces", true, (DataSourceUpdateMode)1));
			((Control)chkSecDecPlaces).set_ForeColor(Color.DarkRed);
			((Control)chkSecDecPlaces).set_Location(new Point(290, 129));
			((Control)chkSecDecPlaces).set_Name("chkSecDecPlaces");
			((Control)chkSecDecPlaces).set_Size(new Size(179, 17));
			((Control)chkSecDecPlaces).set_TabIndex(52);
			((Control)chkSecDecPlaces).set_Text("Display secs to 4 decimal places");
			((ButtonBase)chkSecDecPlaces).set_UseVisualStyleBackColor(true);
			chkSecDecPlaces.add_CheckedChanged((EventHandler)chkSecDecPlaces_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(750, 488));
			((Control)this).get_Controls().Add((Control)(object)chkSecDecPlaces);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)txtFrameID);
			((Control)this).get_Controls().Add((Control)(object)txtMins);
			((Control)this).get_Controls().Add((Control)(object)txtSecs);
			((Control)this).get_Controls().Add((Control)(object)cmdSetTimeUser);
			((Control)this).get_Controls().Add((Control)(object)txtHrs);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnFrameRate);
			((Control)this).get_Controls().Add((Control)(object)label47);
			((Control)this).get_Controls().Add((Control)(object)label45);
			((Control)this).get_Controls().Add((Control)(object)cmdSetTimePAL);
			((Control)this).get_Controls().Add((Control)(object)cmdSetTimeNTSC);
			((Control)this).get_Controls().Add((Control)(object)label46);
			((Control)this).get_Controls().Add((Control)(object)cmdSetTime);
			((Control)this).get_Controls().Add((Control)(object)label44);
			((Control)this).get_Controls().Add((Control)(object)label43);
			((Control)this).get_Controls().Add((Control)(object)label42);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)gridFrameTimes);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lblTangraAdvice);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("AOTA_SetTimeScale");
			((Control)this).set_Text("Set or adjust the time scale manually");
			((Form)this).add_FormClosed(new FormClosedEventHandler(AOTA_SetTimeScale_FormClosed));
			((Form)this).add_Load((EventHandler)AOTA_SetTimeScale_Load);
			((ISupportInitialize)gridFrameTimes).EndInit();
			((ISupportInitialize)updnFrameRate).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
