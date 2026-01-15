using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using Occult;
using Occult.Properties;

namespace AOTA
{
	public class SetIntegrationDetails : Form
	{
		internal int NumberOfFramesBinned_Integrated = 1;

		private int FirstFrame;

		private float Xscale = 1f;

		internal int MaxPlotWidth = 45000;

		internal int MaxPointsToDisplay = 5000;

		internal int PointsToDisplay = 100;

		internal float StepPlotLength = 5f;

		private IContainer components;

		private PictureBox picPlot;

		private NumericUpDown updnFirstFrame;

		private Button cmdOK;

		private Label label1;

		private Label label2;

		private Label label42;

		private NumericUpDown updnPointLength;

		internal NumericUpDown updnNumberOfBins;

		private Panel panel1;

		private Panel panel2;

		private ToolTip toolTip1;

		private Button cmdCancel;

		private Button cmdHelp;

		private Label label3;

		private RadioButton optNoNorm;

		private RadioButton optNorm1;

		private RadioButton optNorm2;

		private RadioButton optNorm3;

		private Panel panel4;

		private Label label4;

		private ComboBox cmbRun;

		private Panel panel5;

		private NumericUpDown updnSetMagDrop;

		private Label label5;

		private Label label6;

		private Panel panelPlot;

		private Label label7;

		private Panel panel3;

		private RadioButton optBack_Normal;

		private Label label8;

		private RadioButton optBack_Ave;

		public SetIntegrationDetails()
		{
			InitializeComponent();
			updnPointLength.set_Value((decimal)StepPlotLength);
			updnNumberOfBins.set_Value((decimal)AOTAData.NumberOfFramesBinned);
			updnFirstFrame.set_Value((decimal)AOTAData.FirstFrameUsedInAnalysis);
			AOTAData.CancelIntegration = false;
			((Control)cmdCancel).set_Enabled(AOTAData.StarCountToPlot > 0);
			optNoNorm.set_Checked(AOTAData.NormalisationStarRef == 0);
			optNorm1.set_Checked(AOTAData.NormalisationStarRef == 1);
			optNorm2.set_Checked(AOTAData.NormalisationStarRef == 2);
			optNorm3.set_Checked(AOTAData.NormalisationStarRef == 3);
			((Control)optNorm1).set_Enabled(AOTAData.Comp1Used);
			((Control)optNorm2).set_Enabled(AOTAData.Comp2Used);
			((Control)optNorm3).set_Enabled(AOTAData.Comp3Used);
			optBack_Normal.set_Checked(AOTAData.Background_Point);
			optBack_Ave.set_Checked(AOTAData.Background_Average);
			((Control)optBack_Ave).set_Enabled(AOTAData.AllowBothPoint_Average);
			ResizeForm();
			MaxPointsToDisplay = (int)((float)MaxPlotWidth / StepPlotLength);
			((ListControl)cmbRun).set_SelectedIndex(1);
			for (int i = 0; i < cmbRun.get_Items().get_Count(); i++)
			{
				if (cmbRun.get_Items().get_Item(i).ToString() == AOTAData.RunningAverage.ToString())
				{
					((ListControl)cmbRun).set_SelectedIndex(i);
				}
			}
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			AOTAData.FirstFrameUsedInAnalysis = FirstFrame;
			AOTAData.NumberOfFramesBinned = NumberOfFramesBinned_Integrated;
			AOTAData.CancelIntegration = false;
			if (optNoNorm.get_Checked())
			{
				AOTAData.NormalisationStarRef = 0;
			}
			else if (optNorm1.get_Checked())
			{
				AOTAData.NormalisationStarRef = 1;
			}
			else if (optNorm2.get_Checked())
			{
				AOTAData.NormalisationStarRef = 2;
			}
			else if (optNorm3.get_Checked())
			{
				AOTAData.NormalisationStarRef = 3;
			}
			AOTAData.Background_Point = optBack_Normal.get_Checked();
			AOTAData.Background_Average = optBack_Ave.get_Checked();
			AOTAData.RunningAverage = int.Parse(cmbRun.get_Items().get_Item(((ListControl)cmbRun).get_SelectedIndex()).ToString());
			if (AOTAData.PlotForm != null)
			{
				NumericUpDown updnExpectedMagDrop = AOTAData.PlotForm.updnExpectedMagDrop;
				decimal value;
				AOTAData.PlotForm.updnMagDrop.set_Value(value = updnSetMagDrop.get_Value());
				updnExpectedMagDrop.set_Value(value);
			}
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("You have selected Cancel.\r\n\r\nIf you have made data changes with the Integrity Checker, \r\nthose changes will not be effective unless you leave this form using 'OK'.\r\n\r\nDo you want to leave this form using 'Cancel'?", "Confirm Cancel", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				AOTAData.CancelIntegration = true;
				((Form)this).Close();
			}
		}

		private void SetIntegrationDetails_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			Plot();
			updnFirstFrame.set_Value((decimal)AOTAData.FirstFrameUsedInAnalysis);
			updnNumberOfBins.set_Value((decimal)NumberOfFramesBinned_Integrated);
			Panel obj = panelPlot;
			int top;
			((Control)panelPlot).set_Left(top = 1);
			((Control)obj).set_Top(top);
			Panel obj2 = panel1;
			Panel obj3 = panel2;
			Panel obj4 = panel3;
			Panel obj5 = panel4;
			Panel obj6 = panel5;
			int num;
			((Control)cmdOK).set_Top(num = ((Control)this).get_Height() - 112);
			int num2;
			((Control)obj6).set_Top(num2 = num);
			int num3;
			((Control)obj5).set_Top(num3 = num2);
			int num4;
			((Control)obj4).set_Top(num4 = num3);
			((Control)obj3).set_Top(top = num4);
			((Control)obj2).set_Top(top);
			((Control)cmdCancel).set_Top(((Control)cmdOK).get_Top() + 22);
			((Control)cmdHelp).set_Top(((Control)cmdOK).get_Top() + 48);
			optBack_Normal.set_Checked(AOTAData.Background_Point);
			optBack_Ave.set_Checked(AOTAData.Background_Average);
			((Control)optBack_Ave).set_Enabled(AOTAData.AllowBothPoint_Average);
			if (AOTAData.PlotForm != null)
			{
				ResizeForm();
				if (AOTAData.PlotForm.updnMagDrop.get_Value() > 0m)
				{
					updnSetMagDrop.set_Value(AOTAData.PlotForm.updnMagDrop.get_Value());
					AOTAData.PlotForm.MagDropCalculator(UpdateMagDrop: false);
				}
			}
		}

		private void updnNumberOfBins_ValueChanged(object sender, EventArgs e)
		{
			NumberOfFramesBinned_Integrated = (int)updnNumberOfBins.get_Value();
			Plot();
		}

		private void updnFirstFrame_ValueChanged(object sender, EventArgs e)
		{
			FirstFrame = (int)updnFirstFrame.get_Value();
			Plot();
		}

		private void Plot()
		{
			float num = 0f;
			float y = 0f;
			float num2 = 0f;
			float num3 = -100f;
			string text = "";
			Pen pen = new Pen(Color.DarkBlue, 3f);
			Pen pen2 = new Pen(Color.LightBlue, 1f);
			Pen pen3 = new Pen(Color.Red, 16f);
			Pen pen4 = new Pen(Color.Salmon, 1f);
			pen4.DashPattern = new float[2] { 2f, 6f };
			Pen pen5 = new Pen(Color.Black, 1f);
			Brush gold = Brushes.Gold;
			Pen pen6 = new Pen(Color.LightGreen, 1f);
			pen6.DashStyle = DashStyle.Dot;
			Font font = new Font("Times New Roman", 9f, FontStyle.Bold);
			int width = ((Control)picPlot).get_Width();
			int height = ((Control)picPlot).get_Height();
			PointsToDisplay = AOTAData.StarCount;
			if (PointsToDisplay > MaxPointsToDisplay)
			{
				PointsToDisplay = MaxPointsToDisplay;
			}
			for (int i = 0; i < PointsToDisplay; i++)
			{
				if (num2 < AOTAData.Star[i])
				{
					num2 = AOTAData.Star[i];
				}
			}
			float num4 = ((num2 != 0f) ? ((float)height / num2 / 1.05f) : 1f);
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			graphics.FillRectangle(gold, (float)FirstFrame * StepPlotLength, 0f, (float)NumberOfFramesBinned_Integrated * StepPlotLength, height);
			float num5 = graphics.MeasureString("1000", font).Width + 5f;
			num = (float)PointsToDisplay * StepPlotLength;
			graphics.DrawLine(pen5, num, 0f, num, height);
			graphics.FillRectangle(Brushes.AliceBlue, num + 1f, 0f, (float)((Control)picPlot).get_Width() - num - 2f, height);
			for (int j = 0; j < PointsToDisplay; j++)
			{
				float num6 = (float)height - num4 * AOTAData.Star[j];
				num = (float)j * StepPlotLength;
				if (j > 0)
				{
					if (!AOTAData.StarValid[j - 1] | !AOTAData.StarValid[j])
					{
						graphics.DrawLine(pen4, num, y, num, num6);
					}
					else
					{
						graphics.DrawLine(pen2, num, y, num, num6);
					}
				}
				if (!AOTAData.StarValid[j])
				{
					graphics.DrawLine(pen3, num, num6, num + StepPlotLength, num6);
				}
				else
				{
					graphics.DrawLine(pen, num, num6, num + StepPlotLength, num6);
				}
				y = num6;
				if (StepPlotLength >= 2f)
				{
					graphics.DrawLine(pen5, num, height, num, height - 5);
				}
				if (NumberOfFramesBinned_Integrated > 1 && (j - FirstFrame) % NumberOfFramesBinned_Integrated == 0)
				{
					graphics.DrawLine(pen6, num, 0f, num, height);
				}
				if (j % 5 == 0)
				{
					graphics.DrawLine(pen5, num, height, num, height - 9);
				}
				if (j % 10 == 0)
				{
					graphics.DrawLine(pen5, num, height, num, height - 12);
					if (num - num3 > num5)
					{
						text = j.ToString().PadLeft(2);
						graphics.DrawString(text, font, Brushes.Black, num + 0.5f * Xscale - graphics.MeasureString(text, font).Width / 2f, height - 28);
						graphics.DrawLine(pen5, num, height, num, height - 15);
						num3 = num;
					}
				}
			}
			graphics.DrawRectangle(pen5, 0, 0, width - 1, height - 1);
			picPlot.set_Image((Image)image);
			graphics.Dispose();
		}

		private void updnPointLength_ValueChanged(object sender, EventArgs e)
		{
			StepPlotLength = (float)updnPointLength.get_Value();
			MaxPointsToDisplay = (int)((float)MaxPlotWidth / StepPlotLength);
			Plot();
		}

		private void picPlot_MouseClick(object sender, MouseEventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Invalid comparison between Unknown and I4
			toolTip1.SetToolTip((Control)(object)picPlot, "Left click to set Start location\r\nRight click to set width");
			float num = e.get_X();
			if ((int)e.get_Button() == 1048576)
			{
				int num2 = (int)(num / Xscale / StepPlotLength);
				updnFirstFrame.set_Value((decimal)(num2 % 255));
				return;
			}
			int num3 = (int)(num / Xscale / StepPlotLength);
			int num4 = (int)updnFirstFrame.get_Value();
			if (num3 > num4)
			{
				updnNumberOfBins.set_Value((decimal)((num3 - num4 + 1) % 255));
				return;
			}
			updnNumberOfBins.set_Value((decimal)((float)(num4 - num3 + 1) / StepPlotLength % 255f));
			updnFirstFrame.set_Value((decimal)((float)num3 / StepPlotLength % 255f));
		}

		private void SetIntegrationDetails_Resize(object sender, EventArgs e)
		{
			ResizeForm();
		}

		private void ResizeForm()
		{
			((Control)panelPlot).set_Width(((Control)this).get_Width() - 20);
			((Control)panelPlot).set_Height(((Control)this).get_Height() - 118);
			((Control)picPlot).set_Width((int)((float)PointsToDisplay * StepPlotLength));
			((Control)picPlot).set_Height(((Control)panelPlot).get_Height() - 17);
			Plot();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA integration and binning");
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
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_026c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0276: Expected O, but got Unknown
			//IL_15e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_15ea: Expected O, but got Unknown
			components = new Container();
			picPlot = new PictureBox();
			updnFirstFrame = new NumericUpDown();
			cmdOK = new Button();
			label1 = new Label();
			label2 = new Label();
			label42 = new Label();
			updnPointLength = new NumericUpDown();
			updnNumberOfBins = new NumericUpDown();
			panel1 = new Panel();
			panel2 = new Panel();
			label6 = new Label();
			toolTip1 = new ToolTip(components);
			cmdCancel = new Button();
			cmdHelp = new Button();
			label3 = new Label();
			optNoNorm = new RadioButton();
			optNorm1 = new RadioButton();
			optNorm2 = new RadioButton();
			optNorm3 = new RadioButton();
			panel4 = new Panel();
			label7 = new Label();
			label4 = new Label();
			cmbRun = new ComboBox();
			panel5 = new Panel();
			updnSetMagDrop = new NumericUpDown();
			label5 = new Label();
			panelPlot = new Panel();
			panel3 = new Panel();
			optBack_Ave = new RadioButton();
			optBack_Normal = new RadioButton();
			label8 = new Label();
			((ISupportInitialize)picPlot).BeginInit();
			((ISupportInitialize)updnFirstFrame).BeginInit();
			((ISupportInitialize)updnPointLength).BeginInit();
			((ISupportInitialize)updnNumberOfBins).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((ISupportInitialize)updnSetMagDrop).BeginInit();
			((Control)panelPlot).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)picPlot).set_BackColor(Color.White);
			((Control)picPlot).set_Location(new Point(0, 0));
			((Control)picPlot).set_Name("picPlot");
			((Control)picPlot).set_Size(new Size(737, 213));
			picPlot.set_TabIndex(0);
			picPlot.set_TabStop(false);
			toolTip1.SetToolTip((Control)(object)picPlot, "Left click to set first frame\r\nRight click to set width");
			((Control)picPlot).add_MouseClick(new MouseEventHandler(picPlot_MouseClick));
			((Control)updnFirstFrame).set_Location(new Point(94, 43));
			updnFirstFrame.set_Maximum(new decimal(new int[4] { 256, 0, 0, 0 }));
			((Control)updnFirstFrame).set_Name("updnFirstFrame");
			((Control)updnFirstFrame).set_Size(new Size(45, 20));
			((Control)updnFirstFrame).set_TabIndex(3);
			updnFirstFrame.add_ValueChanged((EventHandler)updnFirstFrame_ValueChanged);
			((Control)cmdOK).set_Anchor((AnchorStyles)6);
			((Control)cmdOK).set_BackColor(Color.PaleGreen);
			((Control)cmdOK).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOK).set_Location(new Point(703, 213));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(54, 22));
			((Control)cmdOK).set_TabIndex(3);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(false);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(37, 46));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(55, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("First frame");
			label1.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(2, 22));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(90, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("Number of frames");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(0, 0));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(55, 26));
			((Control)label42).set_TabIndex(20);
			((Control)label42).set_Text("1. Point \r\nLength");
			label42.set_TextAlign(ContentAlignment.TopCenter);
			updnPointLength.set_DecimalPlaces(1);
			((Control)updnPointLength).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnPointLength.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnPointLength).set_Location(new Point(10, 34));
			updnPointLength.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnPointLength.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnPointLength).set_Name("updnPointLength");
			((Control)updnPointLength).set_Size(new Size(45, 20));
			((Control)updnPointLength).set_TabIndex(19);
			((UpDownBase)updnPointLength).set_TextAlign((HorizontalAlignment)2);
			updnPointLength.set_Value(new decimal(new int[4] { 5, 0, 0, 0 }));
			updnPointLength.add_ValueChanged((EventHandler)updnPointLength_ValueChanged);
			((Control)updnNumberOfBins).set_Location(new Point(94, 19));
			updnNumberOfBins.set_Maximum(new decimal(new int[4] { 256, 0, 0, 0 }));
			updnNumberOfBins.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnNumberOfBins).set_Name("updnNumberOfBins");
			((Control)updnNumberOfBins).set_Size(new Size(51, 20));
			((Control)updnNumberOfBins).set_TabIndex(1);
			updnNumberOfBins.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnNumberOfBins.add_ValueChanged((EventHandler)updnNumberOfBins_ValueChanged);
			((Control)panel1).set_Anchor((AnchorStyles)6);
			((Control)panel1).set_BackColor(Color.Honeydew);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label42);
			((Control)panel1).get_Controls().Add((Control)(object)updnPointLength);
			((Control)panel1).set_Location(new Point(2, 213));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(72, 71));
			((Control)panel1).set_TabIndex(22);
			((Control)panel2).set_Anchor((AnchorStyles)6);
			((Control)panel2).set_BackColor(Color.LavenderBlush);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label2);
			((Control)panel2).get_Controls().Add((Control)(object)label6);
			((Control)panel2).get_Controls().Add((Control)(object)updnNumberOfBins);
			((Control)panel2).get_Controls().Add((Control)(object)label1);
			((Control)panel2).get_Controls().Add((Control)(object)updnFirstFrame);
			((Control)panel2).set_Location(new Point(158, 213));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(150, 71));
			((Control)panel2).set_TabIndex(23);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(0, 0));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(130, 13));
			((Control)label6).set_TabIndex(22);
			((Control)label6).set_Text("3. Binning/integration");
			label6.set_TextAlign(ContentAlignment.MiddleCenter);
			toolTip1.set_AutoPopDelay(3000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_ReshowDelay(100);
			toolTip1.set_Tag((object)"");
			((Control)cmdCancel).set_Anchor((AnchorStyles)6);
			((Control)cmdCancel).set_BackColor(Color.LightSalmon);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(703, 235));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(54, 26));
			((Control)cmdCancel).set_TabIndex(25);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdHelp).set_Anchor((AnchorStyles)6);
			((Control)cmdHelp).set_BackColor(Color.LightSkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdHelp).set_ForeColor(Color.Black);
			((Control)cmdHelp).set_Location(new Point(703, 261));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(54, 22));
			((Control)cmdHelp).set_TabIndex(26);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(0, 0));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(133, 17));
			((Control)label3).set_TabIndex(27);
			((Control)label3).set_Text("4. Normalise to:  ");
			((Control)optNoNorm).set_AutoSize(true);
			optNoNorm.set_Checked(true);
			((Control)optNoNorm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNoNorm).set_ForeColor(Color.Blue);
			((Control)optNoNorm).set_Location(new Point(142, 5));
			((Control)optNoNorm).set_Name("optNoNorm");
			((Control)optNoNorm).set_Size(new Size(55, 17));
			((Control)optNoNorm).set_TabIndex(28);
			optNoNorm.set_TabStop(true);
			((Control)optNoNorm).set_Text("None");
			((ButtonBase)optNoNorm).set_UseVisualStyleBackColor(true);
			((Control)optNorm1).set_AutoSize(true);
			((Control)optNorm1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNorm1).set_ForeColor(Color.MediumSeaGreen);
			((Control)optNorm1).set_Location(new Point(142, 20));
			((Control)optNorm1).set_Name("optNorm1");
			((Control)optNorm1).set_Size(new Size(134, 17));
			((Control)optNorm1).set_TabIndex(29);
			optNorm1.set_TabStop(true);
			((Control)optNorm1).set_Text("Comparison star #1");
			((ButtonBase)optNorm1).set_UseVisualStyleBackColor(true);
			((Control)optNorm2).set_AutoSize(true);
			((Control)optNorm2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNorm2).set_ForeColor(Color.DarkOrange);
			((Control)optNorm2).set_Location(new Point(142, 35));
			((Control)optNorm2).set_Name("optNorm2");
			((Control)optNorm2).set_Size(new Size(134, 17));
			((Control)optNorm2).set_TabIndex(30);
			optNorm2.set_TabStop(true);
			((Control)optNorm2).set_Text("Comparison star #2");
			((ButtonBase)optNorm2).set_UseVisualStyleBackColor(true);
			((Control)optNorm3).set_AutoSize(true);
			((Control)optNorm3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNorm3).set_ForeColor(Color.MediumVioletRed);
			((Control)optNorm3).set_Location(new Point(142, 51));
			((Control)optNorm3).set_Name("optNorm3");
			((Control)optNorm3).set_Size(new Size(134, 17));
			((Control)optNorm3).set_TabIndex(31);
			optNorm3.set_TabStop(true);
			((Control)optNorm3).set_Text("Comparison star #3");
			((ButtonBase)optNorm3).set_UseVisualStyleBackColor(true);
			((Control)panel4).set_Anchor((AnchorStyles)6);
			((Control)panel4).set_BackColor(Color.Snow);
			panel4.set_BorderStyle((BorderStyle)1);
			((Control)panel4).get_Controls().Add((Control)(object)label7);
			((Control)panel4).get_Controls().Add((Control)(object)label4);
			((Control)panel4).get_Controls().Add((Control)(object)cmbRun);
			((Control)panel4).get_Controls().Add((Control)(object)optNorm3);
			((Control)panel4).get_Controls().Add((Control)(object)optNorm2);
			((Control)panel4).get_Controls().Add((Control)(object)optNorm1);
			((Control)panel4).get_Controls().Add((Control)(object)optNoNorm);
			((Control)panel4).get_Controls().Add((Control)(object)label3);
			((Control)panel4).set_Location(new Point(313, 213));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(280, 71));
			((Control)panel4).set_TabIndex(32);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(84, 49));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(30, 13));
			((Control)label7).set_TabIndex(34);
			((Control)label7).set_Text("bins");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(10, 29));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(119, 13));
			((Control)label4).set_TabIndex(33);
			((Control)label4).set_Text("Running average of");
			((ListControl)cmbRun).set_FormattingEnabled(true);
			cmbRun.get_Items().AddRange(new object[10] { "1", "3", "5", "9", "16", "24", "32", "48", "64", "96" });
			((Control)cmbRun).set_Location(new Point(37, 45));
			((Control)cmbRun).set_Name("cmbRun");
			((Control)cmbRun).set_Size(new Size(43, 21));
			((Control)cmbRun).set_TabIndex(32);
			((Control)panel5).set_Anchor((AnchorStyles)6);
			((Control)panel5).set_BackColor(Color.PaleTurquoise);
			panel5.set_BorderStyle((BorderStyle)1);
			((Control)panel5).get_Controls().Add((Control)(object)updnSetMagDrop);
			((Control)panel5).get_Controls().Add((Control)(object)label5);
			((Control)panel5).set_Location(new Point(79, 213));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(74, 71));
			((Control)panel5).set_TabIndex(33);
			updnSetMagDrop.set_DecimalPlaces(1);
			updnSetMagDrop.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnSetMagDrop).set_Location(new Point(15, 34));
			updnSetMagDrop.set_Maximum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnSetMagDrop).set_Name("updnSetMagDrop");
			((Control)updnSetMagDrop).set_Size(new Size(42, 20));
			((Control)updnSetMagDrop).set_TabIndex(7);
			((UpDownBase)updnSetMagDrop).set_TextAlign((HorizontalAlignment)2);
			updnSetMagDrop.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)label5).set_Anchor((AnchorStyles)6);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(0, 0));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(75, 26));
			((Control)label5).set_TabIndex(6);
			((Control)label5).set_Text("2. Expected\r\nMag. drop");
			((Control)panelPlot).set_Anchor((AnchorStyles)6);
			((ScrollableControl)panelPlot).set_AutoScroll(true);
			((Control)panelPlot).get_Controls().Add((Control)(object)picPlot);
			((Control)panelPlot).set_Location(new Point(0, -44));
			((Control)panelPlot).set_Name("panelPlot");
			((Control)panelPlot).set_Size(new Size(761, 233));
			((Control)panelPlot).set_TabIndex(34);
			((Control)panel3).set_Anchor((AnchorStyles)6);
			((Control)panel3).set_BackColor(Color.LemonChiffon);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)optBack_Ave);
			((Control)panel3).get_Controls().Add((Control)(object)optBack_Normal);
			((Control)panel3).get_Controls().Add((Control)(object)label8);
			((Control)panel3).set_Location(new Point(598, 213));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(99, 71));
			((Control)panel3).set_TabIndex(35);
			((Control)optBack_Ave).set_AutoSize(true);
			((Control)optBack_Ave).set_Location(new Point(3, 43));
			((Control)optBack_Ave).set_Name("optBack_Ave");
			((Control)optBack_Ave).set_Size(new Size(52, 17));
			((Control)optBack_Ave).set_TabIndex(4);
			((Control)optBack_Ave).set_Text("Mean");
			((ButtonBase)optBack_Ave).set_UseVisualStyleBackColor(true);
			((Control)optBack_Normal).set_AutoSize(true);
			optBack_Normal.set_Checked(true);
			((Control)optBack_Normal).set_Location(new Point(3, 20));
			((Control)optBack_Normal).set_Name("optBack_Normal");
			((Control)optBack_Normal).set_Size(new Size(89, 17));
			((Control)optBack_Normal).set_TabIndex(1);
			optBack_Normal.set_TabStop(true);
			((Control)optBack_Normal).set_Text("Point by point");
			((ButtonBase)optBack_Normal).set_UseVisualStyleBackColor(true);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(0, 0));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(90, 13));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("5. Background");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(Settings.Default.BinFormSize);
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panelPlot);
			((Control)this).get_Controls().Add((Control)(object)panel5);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_DataBindings().Add(new Binding("ClientSize", (object)Settings.Default, "BinFormSize", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)6);
			((Control)this).set_MaximumSize(new Size(2000, 370));
			((Control)this).set_MinimumSize(new Size(780, 370));
			((Control)this).set_Name("SetIntegrationDetails");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("SET      Integration + Binning,    Normalisation,    Expected magnitude drop");
			((Form)this).add_Load((EventHandler)SetIntegrationDetails_Load);
			((Control)this).add_Resize((EventHandler)SetIntegrationDetails_Resize);
			((ISupportInitialize)picPlot).EndInit();
			((ISupportInitialize)updnFirstFrame).EndInit();
			((ISupportInitialize)updnPointLength).EndInit();
			((ISupportInitialize)updnNumberOfBins).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((ISupportInitialize)updnSetMagDrop).EndInit();
			((Control)panelPlot).ResumeLayout(false);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
