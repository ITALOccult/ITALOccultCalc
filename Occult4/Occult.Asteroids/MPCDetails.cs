using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.MPC_PDS;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class MPCDetails : Form
	{
		private IContainer components;

		private NumericUpDown updnYear;

		private ComboBox cmbDay;

		private ComboBox cmbMonth;

		private Label label1;

		private TextBox txtMPCNumber;

		private Button cmdCancel;

		private Button cmdAddIDs;

		private Button cmdHelp;

		private RadioButton optPublished;

		private RadioButton optSubmitted;

		private Label label3;

		private Panel panel1;

		private Label label4;

		private ListBox lstMPC_ID;

		private Button cmdPasteIDs;

		private Label label2;

		private Button cmdAddMPC;

		private Panel pnlDate;

		private Label lblIDnumber;

		internal ProgressBar PBar;

		public MPCDetails()
		{
			InitializeComponent();
		}

		private void MPCDetails_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			Data_and_Plots.CancelDetails = false;
			updnYear.set_Value((decimal)DateTime.Now.Year);
			((ListControl)cmbMonth).set_SelectedIndex(DateTime.Now.Month - 1);
			((ListControl)cmbDay).set_SelectedIndex(DateTime.Now.Day - 1);
			((Control)optSubmitted).Focus();
		}

		private void MPCDetails_FormClosing(object sender, FormClosingEventArgs e)
		{
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			Data_and_Plots.CancelDetails = true;
			((Form)this).Close();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - set MPC details");
		}

		private void optPublished_Click(object sender, EventArgs e)
		{
			optPublished.set_Checked(true);
			optSubmitted.set_Checked(false);
			((Control)txtMPCNumber).set_Enabled(optPublished.get_Checked());
		}

		private void optSubmitted_Click(object sender, EventArgs e)
		{
			optPublished.set_Checked(false);
			optSubmitted.set_Checked(true);
			((Control)txtMPCNumber).set_Enabled(optPublished.get_Checked());
		}

		private void cmdPasteIDs_Click(object sender, EventArgs e)
		{
			Data_and_Plots.MPCdetailsList = new List<SubmissionID>();
			lstMPC_ID.get_Items().Clear();
			string[] array = Clipboard.GetText().Replace("\r", "").Split(new char[1] { '\n' });
			bool flag = array[0].Contains("obs80");
			for (int i = 0; i < array.Length; i++)
			{
				SubmissionID submissionID = new SubmissionID();
				if (flag)
				{
					if (submissionID.DecodeBackcapture(array[i]))
					{
						Data_and_Plots.MPCdetailsList.Add(submissionID);
					}
				}
				else if (submissionID.Decode(array[i]))
				{
					Data_and_Plots.MPCdetailsList.Add(submissionID);
				}
			}
			Data_and_Plots.MPCdetailsList.Sort();
			for (int j = 0; j < Data_and_Plots.MPCdetailsList.Count; j++)
			{
				lstMPC_ID.get_Items().Add((object)Data_and_Plots.MPCdetailsList[j].ToString());
			}
			((Control)lblIDnumber).set_Text(lstMPC_ID.get_Items().get_Count() + " observations to match");
		}

		private void optSubmitted_CheckedChanged(object sender, EventArgs e)
		{
			Button obj = cmdAddIDs;
			bool @checked;
			((Control)pnlDate).set_Enabled(@checked = optSubmitted.get_Checked());
			((Control)obj).set_Enabled(@checked);
			((Control)cmdAddMPC).set_Enabled(!optSubmitted.get_Checked());
		}

		private void optPublished_CheckedChanged(object sender, EventArgs e)
		{
			Button obj = cmdAddIDs;
			bool enabled;
			((Control)pnlDate).set_Enabled(enabled = !optPublished.get_Checked());
			((Control)obj).set_Enabled(enabled);
			((Control)cmdAddMPC).set_Enabled(optPublished.get_Checked());
		}

		private void cmdAddMPC_Click(object sender, EventArgs e)
		{
			Data_and_Plots.AddMPCdata_toHistoricalFile(AddID: false, AddMPC: true, "", ((Control)txtMPCNumber).get_Text().Trim());
			lstMPC_ID.get_Items().Clear();
			for (int i = 0; i < Data_and_Plots.MPCdetailsList.Count; i++)
			{
				lstMPC_ID.get_Items().Add((object)Data_and_Plots.MPCdetailsList[i].ToString());
			}
		}

		private void cmdAddIDs_Click(object sender, EventArgs e)
		{
			string submissionDate = updnYear.get_Value() + (((ListControl)cmbMonth).get_SelectedIndex() + 1).ToString().PadLeft(2) + (((ListControl)cmbDay).get_SelectedIndex() + 1).ToString().PadLeft(2);
			Data_and_Plots.AddMPCdata_toHistoricalFile(AddID: true, AddMPC: false, submissionDate, "");
			lstMPC_ID.get_Items().Clear();
			string text = DateTime.Now.Year + Utilities.ShortMonths[DateTime.Now.Month] + DateTime.Now.Day.ToString().PadLeft(2, '0');
			int num = 0;
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Asteroid\\Results\\MPC Files\\Reported psv reports\\UnmatchedAcknowledged " + text + ".txt");
			for (int i = 0; i < Data_and_Plots.MPCdetailsList.Count; i++)
			{
				lstMPC_ID.get_Items().Add((object)Data_and_Plots.MPCdetailsList[i].ToString());
				if (Data_and_Plots.MPCdetailsList[i].StatusValue_None_Good_Bad == 2)
				{
					num++;
					streamWriter.WriteLine(string.Format("{0, 4}", num) + Data_and_Plots.MPCdetailsList[i].ToString());
					if (num % 5 == 0)
					{
						streamWriter.WriteLine("");
					}
				}
			}
			if (num == 0)
			{
				streamWriter.WriteLine(string.Format("MPC data has been added for {0,1} events", Data_and_Plots.MPCdetailsList.Count));
			}
			else
			{
				streamWriter.WriteLine("\r\nMPC data added for {0,1} entries. Data has not been added for the above {1,1} entries", Data_and_Plots.MPCdetailsList.Count - num, num);
			}
		}

		private void MPCDetails_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(544);
			if (((Control)this).get_Height() < 420)
			{
				((Control)this).set_Height(420);
			}
			((Control)lstMPC_ID).set_Height(((Control)this).get_Height() - 248);
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
			//IL_0e9b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ea5: Expected O, but got Unknown
			updnYear = new NumericUpDown();
			cmbDay = new ComboBox();
			cmbMonth = new ComboBox();
			label1 = new Label();
			txtMPCNumber = new TextBox();
			cmdCancel = new Button();
			cmdAddIDs = new Button();
			cmdHelp = new Button();
			optPublished = new RadioButton();
			optSubmitted = new RadioButton();
			label3 = new Label();
			panel1 = new Panel();
			label2 = new Label();
			label4 = new Label();
			lstMPC_ID = new ListBox();
			cmdPasteIDs = new Button();
			cmdAddMPC = new Button();
			pnlDate = new Panel();
			lblIDnumber = new Label();
			PBar = new ProgressBar();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)pnlDate).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)updnYear).set_Location(new Point(135, 6));
			updnYear.set_Maximum(new decimal(new int[4] { 2050, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 2007, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(3);
			updnYear.set_Value(new decimal(new int[4] { 2025, 0, 0, 0 }));
			cmbDay.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDay).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmbDay).set_ForeColor(Color.Blue);
			((ListControl)cmbDay).set_FormattingEnabled(true);
			cmbDay.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbDay).set_Location(new Point(245, 6));
			cmbDay.set_MaxDropDownItems(31);
			((Control)cmbDay).set_Name("cmbDay");
			((Control)cmbDay).set_Size(new Size(40, 21));
			((Control)cmbDay).set_TabIndex(5);
			cmbMonth.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMonth).set_FormattingEnabled(true);
			cmbMonth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth).set_Location(new Point(196, 6));
			((Control)cmbMonth).set_Name("cmbMonth");
			((Control)cmbMonth).set_Size(new Size(43, 21));
			((Control)cmbMonth).set_TabIndex(4);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(48, 49));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(260, 13));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("Add Minor Planet Circular number for the observations");
			((Control)txtMPCNumber).set_Enabled(false);
			((Control)txtMPCNumber).set_Location(new Point(317, 46));
			((Control)txtMPCNumber).set_Name("txtMPCNumber");
			((Control)txtMPCNumber).set_Size(new Size(79, 20));
			((Control)txtMPCNumber).set_TabIndex(2);
			((Control)cmdCancel).set_Anchor((AnchorStyles)6);
			((Control)cmdCancel).set_Location(new Point(393, 335));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(78, 34));
			((Control)cmdCancel).set_TabIndex(7);
			((Control)cmdCancel).set_Text("Exit");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdAddIDs).set_Anchor((AnchorStyles)6);
			((Control)cmdAddIDs).set_Location(new Point(58, 335));
			((Control)cmdAddIDs).set_Name("cmdAddIDs");
			((Control)cmdAddIDs).set_Size(new Size(99, 34));
			((Control)cmdAddIDs).set_TabIndex(6);
			((Control)cmdAddIDs).set_Text("Add \r\nObservation ID's");
			((ButtonBase)cmdAddIDs).set_UseVisualStyleBackColor(true);
			((Control)cmdAddIDs).add_Click((EventHandler)cmdAddIDs_Click);
			((Control)cmdHelp).set_BackColor(Color.LightSkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(441, 1));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(67, 35));
			((Control)cmdHelp).set_TabIndex(8);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			optPublished.set_AutoCheck(false);
			((Control)optPublished).set_AutoSize(true);
			((Control)optPublished).set_Location(new Point(32, 49));
			((Control)optPublished).set_Name("optPublished");
			((Control)optPublished).set_Size(new Size(14, 13));
			((Control)optPublished).set_TabIndex(1);
			((ButtonBase)optPublished).set_UseVisualStyleBackColor(true);
			optPublished.add_CheckedChanged((EventHandler)optPublished_CheckedChanged);
			((Control)optPublished).add_Click((EventHandler)optPublished_Click);
			optSubmitted.set_AutoCheck(false);
			((Control)optSubmitted).set_AutoSize(true);
			optSubmitted.set_Checked(true);
			((Control)optSubmitted).set_Location(new Point(32, 8));
			((Control)optSubmitted).set_Name("optSubmitted");
			((Control)optSubmitted).set_Size(new Size(152, 17));
			((Control)optSubmitted).set_TabIndex(0);
			optSubmitted.set_TabStop(true);
			((Control)optSubmitted).set_Text("Add MPC  Observations ID");
			((ButtonBase)optSubmitted).set_UseVisualStyleBackColor(true);
			optSubmitted.add_CheckedChanged((EventHandler)optSubmitted_CheckedChanged);
			((Control)optSubmitted).add_Click((EventHandler)optSubmitted_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(134, 26));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(47, 17));
			((Control)label3).set_TabIndex(9);
			((Control)label3).set_Text("-  or  -");
			((Control)panel1).set_BackColor(Color.LightCyan);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)PBar);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)optSubmitted);
			((Control)panel1).get_Controls().Add((Control)(object)optPublished);
			((Control)panel1).get_Controls().Add((Control)(object)cmdHelp);
			((Control)panel1).get_Controls().Add((Control)(object)txtMPCNumber);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(-1, 0));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(528, 81));
			((Control)panel1).set_TabIndex(12);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(320, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(68, 13));
			((Control)label2).set_TabIndex(11);
			((Control)label2).set_Text("MPC number");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.Blue);
			((Control)label4).set_Location(new Point(7, 10));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(122, 13));
			((Control)label4).set_TabIndex(13);
			((Control)label4).set_Text("Date of MPC submission");
			((Control)lstMPC_ID).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMPC_ID).set_FormattingEnabled(true);
			lstMPC_ID.set_ItemHeight(14);
			((Control)lstMPC_ID).set_Location(new Point(2, 116));
			((Control)lstMPC_ID).set_Name("lstMPC_ID");
			lstMPC_ID.set_ScrollAlwaysVisible(true);
			((Control)lstMPC_ID).set_Size(new Size(522, 172));
			((Control)lstMPC_ID).set_TabIndex(14);
			((Control)cmdPasteIDs).set_BackColor(Color.PaleGreen);
			((Control)cmdPasteIDs).set_Location(new Point(95, 85));
			((Control)cmdPasteIDs).set_Name("cmdPasteIDs");
			((Control)cmdPasteIDs).set_Size(new Size(154, 26));
			((Control)cmdPasteIDs).set_TabIndex(15);
			((Control)cmdPasteIDs).set_Text("Paste Observation ID's");
			((ButtonBase)cmdPasteIDs).set_UseVisualStyleBackColor(false);
			((Control)cmdPasteIDs).add_Click((EventHandler)cmdPasteIDs_Click);
			((Control)cmdAddMPC).set_Anchor((AnchorStyles)6);
			((Control)cmdAddMPC).set_Enabled(false);
			((Control)cmdAddMPC).set_Location(new Point(199, 335));
			((Control)cmdAddMPC).set_Name("cmdAddMPC");
			((Control)cmdAddMPC).set_Size(new Size(99, 34));
			((Control)cmdAddMPC).set_TabIndex(16);
			((Control)cmdAddMPC).set_Text("Add \r\nMPC number");
			((ButtonBase)cmdAddMPC).set_UseVisualStyleBackColor(true);
			((Control)cmdAddMPC).add_Click((EventHandler)cmdAddMPC_Click);
			((Control)pnlDate).set_Anchor((AnchorStyles)6);
			((Control)pnlDate).get_Controls().Add((Control)(object)label4);
			((Control)pnlDate).get_Controls().Add((Control)(object)cmbDay);
			((Control)pnlDate).get_Controls().Add((Control)(object)cmbMonth);
			((Control)pnlDate).get_Controls().Add((Control)(object)updnYear);
			((Control)pnlDate).set_Location(new Point(115, 293));
			((Control)pnlDate).set_Name("pnlDate");
			((Control)pnlDate).set_Size(new Size(298, 32));
			((Control)pnlDate).set_TabIndex(17);
			((Control)lblIDnumber).set_AutoSize(true);
			((Control)lblIDnumber).set_Location(new Point(353, 93));
			((Control)lblIDnumber).set_Name("lblIDnumber");
			((Control)lblIDnumber).set_Size(new Size(120, 13));
			((Control)lblIDnumber).set_TabIndex(18);
			((Control)lblIDnumber).set_Text("0 observations to match");
			lblIDnumber.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)PBar).set_Location(new Point(225, 4));
			((Control)PBar).set_Name("PBar");
			((Control)PBar).set_Size(new Size(177, 10));
			((Control)PBar).set_TabIndex(12);
			((Control)PBar).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(528, 377));
			((Control)this).get_Controls().Add((Control)(object)lblIDnumber);
			((Control)this).get_Controls().Add((Control)(object)pnlDate);
			((Control)this).get_Controls().Add((Control)(object)cmdAddMPC);
			((Control)this).get_Controls().Add((Control)(object)cmdPasteIDs);
			((Control)this).get_Controls().Add((Control)(object)lstMPC_ID);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdAddIDs);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterMPCDetails", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)6);
			((Form)this).set_HelpButton(true);
			((Form)this).set_Location(Settings.Default.LocationAsterMPCDetails);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("MPCDetails");
			((Control)this).set_Text("Set details of publication in Minor Planet Circulars");
			((Form)this).add_Load((EventHandler)MPCDetails_Load);
			((Control)this).add_Resize((EventHandler)MPCDetails_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)pnlDate).ResumeLayout(false);
			((Control)pnlDate).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
