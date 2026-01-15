using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using LightCurves;
using Occult;
using Occult.Properties;

namespace AOTA
{
	public class IntegrityCheck : Form
	{
		private int GridTimesSelectedRow;

		private bool AddingRows;

		private int FoldHeight = 60;

		private float Xscale = 4f;

		private int NumPlotPoints = 512;

		private bool GetToolTip = true;

		private bool IsCorrectingOCR;

		private static DisplayData DisplayOCR = new DisplayData();

		private static string OCRmessage = "";

		private IContainer components;

		internal DataGridView gridFrameTimes;

		private PictureBox picFold;

		private ListBox lstDropped;

		private ListBox lstDuplicates;

		private Label label1;

		private Label label2;

		private TextBox txtFrameID;

		private TextBox txtMins;

		private TextBox txtSecs;

		private TextBox txtHrs;

		private Button cmdSetTime;

		private Label label44;

		private Label label43;

		private Label label42;

		private Label label7;

		private Button cmdCheckDroppedDuplicates;

		private Button cmdCheckMeasures;

		private Button cmdPlotFolded;

		private ListBox lstDuplicates2;

		private Label label3;

		private Button cmdInsertDroppedFrames;

		private Label label4;

		private NumericUpDown updnFirstFrame;

		private Label label5;

		private ComboBox cmbNumInteg;

		private MenuStrip menuStrip1;

		private Label label6;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Panel panelPlot;

		private ToolTip toolTip1;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private CheckBox chkFoldShort;

		private Button cmdSetTimesDropped;

		private Button cmdClearTimesDropped;

		private Button cmdClearTimesDuplicated;

		private Button cmdSetTimesDuplicated;

		private Button cmdClearMeasuresDuplicated;

		private Button cmdSetMeasuresDuplicated;

		private Button ClearAllChecks;

		private Panel panel1;

		private Panel panel2;

		private DataGridViewTextBoxColumn Frame;

		private DataGridViewTextBoxColumn UTC;

		private DataGridViewTextBoxColumn Valid;

		private DataGridViewTextBoxColumn Dropped;

		private DataGridViewTextBoxColumn Delete;

		private Button cmdExit;

		private ToolStripMenuItem viewOCRCorrectionsToolStripMenuItem;

		private ToolStripMenuItem autoCheckToolStripMenuItem;

		private ToolStripMenuItem viewCorrectionsToolStripMenuItem;

		private ToolStripMenuItem checkOCRForErrorsToolStripMenuItem;

		private Label lblOCRprogress;

		private Label lblFrameRate;

		private CheckBox chkSecDecPlaces;

		public IntegrityCheck()
		{
			InitializeComponent();
		}

		private void IntegrityCheck_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			LoadGridTable();
			((ListControl)cmbNumInteg).set_SelectedIndex(0);
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				((Control)this).set_Text("Integrity check : " + AOTAData.TangraSourceFile);
			}
			else
			{
				((Control)this).set_Text("Integrity check : " + AOTAData.CurrentFileName);
			}
			autoCheckToolStripMenuItem.set_Checked(Settings.Default.OCR_Automatic);
			((Form)this).set_TopMost(true);
		}

		private void LoadGridTable()
		{
			gridFrameTimes.get_Rows().Clear();
			AddingRows = true;
			int decimalLength = 3;
			if (chkSecDecPlaces.get_Checked())
			{
				decimalLength = 4;
			}
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				string text = "X";
				if (AOTAData.StarValid[i])
				{
					text = "";
				}
				gridFrameTimes.get_Rows().Add(new object[5]
				{
					AOTAData.FrameID[i].ToString(),
					Utilities.DEGtoDMS(AOTAData.FrameTimeSecs[i] / 3600.0, 2, decimalLength, MinutesOnly: false),
					text,
					"",
					""
				});
				if (!AOTAData.StarValid[i])
				{
					gridFrameTimes.get_Rows().get_Item(i).get_Cells()
						.get_Item(1)
						.get_Style()
						.set_BackColor(Color.Orange);
				}
			}
			AddingRows = false;
			double FrameRateVariationPercent = 0.0;
			if (AOTAData.TimePresentInSourceData)
			{
				TestForDroppedDuplicatedFrames(out FrameRateVariationPercent);
				((Control)lblFrameRate).set_Text(string.Format("Variation in the interval between frames = {0,1:f3} secs", FrameRateVariationPercent));
				if (FrameRateVariationPercent > 0.002)
				{
					((Control)lblFrameRate).set_ForeColor(Color.Maroon);
					((Control)lblFrameRate).set_BackColor(Color.LightGreen);
				}
			}
			else
			{
				((Control)lblFrameRate).set_Text("");
				((Control)lblFrameRate).set_ForeColor(Color.Black);
				((Control)lblFrameRate).set_BackColor(SystemColors.Control);
				CheckMeasures();
			}
		}

		internal void TestForDroppedFrames(out double FrameRateVariation_Secs)
		{
			//IL_0078: Unknown result type (might be due to invalid IL or missing references)
			lstDropped.get_Items().Clear();
			if (GetNext20ValidTimeIntervals(5, out var _, out var FrameDiff, out FrameRateVariation_Secs))
			{
				double num = FrameDiff / 2.0;
				for (int i = 1; i < AOTAData.StarCount; i++)
				{
					if (Math.Abs(AOTAData.FrameTimeSecs[i] - AOTAData.FrameTimeSecs[i - 1] - FrameDiff) > num)
					{
						lstDropped.get_Items().Add((object)AOTAData.FrameID[i]);
					}
				}
			}
			else
			{
				MessageBox.Show("The time scale is totally unreliable");
			}
		}

		internal void TestForDuplicatedFrames()
		{
			lstDuplicates.get_Items().Clear();
			for (int i = 1; i < AOTAData.StarCount; i++)
			{
				if (AOTAData.FrameTimeSecs[i] == AOTAData.FrameTimeSecs[i - 1])
				{
					lstDuplicates.get_Items().Add((object)AOTAData.FrameID[i]);
				}
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

		private void gridFrameTimes_RowEnter(object sender, DataGridViewCellEventArgs e)
		{
			GridTimesSelectedRow = e.get_RowIndex();
			if (!AddingRows)
			{
				DisplayTimeForSelectedRow();
			}
		}

		private void DisplayTimeForSelectedRow()
		{
			((Control)txtFrameID).set_Text(gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(0)
				.get_Value()
				.ToString());
			string text = gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(1)
				.get_Value()
				.ToString();
			if (text.Trim() == "0")
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
			Display(result);
		}

		private void cmdCheckDroppedDuplicates_Click(object sender, EventArgs e)
		{
			TestForDroppedDuplicatedFrames(out var _);
		}

		private void TestForDroppedDuplicatedFrames(out double FrameRateVariationPercent)
		{
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0083: Unknown result type (might be due to invalid IL or missing references)
			//IL_008d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0093: Unknown result type (might be due to invalid IL or missing references)
			//IL_0099: Invalid comparison between Unknown and I4
			//IL_00af: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Invalid comparison between Unknown and I4
			Application.DoEvents();
			TestForDroppedFrames(out FrameRateVariationPercent);
			TestForDuplicatedFrames();
			if (IsCorrectingOCR || !((lstDropped.get_Items().get_Count() > 5) | (lstDuplicates.get_Items().get_Count() > 5)))
			{
				return;
			}
			if (Settings.Default.OCR_Automatic & !AOTAData.DataHasIntegrityIssues)
			{
				CorrectOCR();
				return;
			}
			string text = "Do you want to check the validity of the OCR'd time stamp values?";
			MessageBoxDefaultButton val = (MessageBoxDefaultButton)0;
			if (AOTAData.DataHasIntegrityIssues)
			{
				text += "\r\n\r\nNOTE: The earlier integrity check of the data read or pasted\r\nindicated there were problems with the integrity of that data.\r\n\r\nIf the integrity issues are with the EVENT TIME data, or \r\nmissing TARGET STAR BRIGHTNESS, you should manually\r\namend the file being read or data being pasted, to\r\ncorrect those errors.\r\n\r\nOtherwise the program might hang if you proceed to check the OCR data.\r\n";
				val = (MessageBoxDefaultButton)256;
			}
			if ((int)MessageBox.Show(text, "OCR errors  :  Check for OCR time errors", (MessageBoxButtons)4, (MessageBoxIcon)32, val, (MessageBoxOptions)2097152) != 6)
			{
				return;
			}
			if (AOTAData.DataHasIntegrityIssues)
			{
				if ((int)MessageBox.Show("The data has integrity issues. Are you sure you want to check for OCR issues?\r\n\r\nThe program might hang if you do the check.", "OCR errors  :  Check for OCR time errors", (MessageBoxButtons)4, (MessageBoxIcon)16, val, (MessageBoxOptions)2097152) == 6)
				{
					CorrectOCR();
				}
			}
			else
			{
				CorrectOCR();
			}
		}

		private void lstDropped_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			string text = lstDropped.get_Items().get_Item(((ListControl)lstDropped).get_SelectedIndex()).ToString();
			for (int num = AOTAData.StarCount - 1; num > 0; num--)
			{
				if (((int)AOTAData.FrameID[num]).ToString() == text)
				{
					gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(1));
					break;
				}
			}
		}

		private void lstDuplicates_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			string text = lstDuplicates.get_Items().get_Item(((ListControl)lstDuplicates).get_SelectedIndex()).ToString();
			for (int i = 1; i < AOTAData.StarCount; i++)
			{
				if (((int)AOTAData.FrameID[i]).ToString() == text)
				{
					gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(i).get_Cells()
						.get_Item(1));
					break;
				}
			}
		}

		private void lstDuplicates2_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			string text = lstDuplicates2.get_Items().get_Item(((ListControl)lstDuplicates2).get_SelectedIndex()).ToString();
			for (int i = 1; i < AOTAData.StarCount; i++)
			{
				if (((int)AOTAData.FrameID[i]).ToString() == text)
				{
					gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(i).get_Cells()
						.get_Item(1));
					break;
				}
			}
		}

		private void gridFrameTimes_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			HitTestInfo val = gridFrameTimes.HitTest(e.get_X(), e.get_Y());
			GridTimesSelectedRow = val.get_RowIndex();
			int columnIndex = val.get_ColumnIndex();
			gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(columnIndex));
			if (columnIndex <= 1)
			{
				return;
			}
			if (gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(columnIndex)
				.get_Value()
				.ToString() == "")
			{
				switch (columnIndex)
				{
				case 3:
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(3)
						.set_Value((object)"1");
					break;
				case 4:
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(4)
						.set_Value((object)"D");
					break;
				default:
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(columnIndex)
						.set_Value((object)"X");
					break;
				}
				switch (columnIndex)
				{
				case 2:
					AOTAData.StarValid[GridTimesSelectedRow] = false;
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(1)
						.get_Style()
						.set_BackColor(Color.Orange);
					break;
				case 3:
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(1)
						.get_Style()
						.set_BackColor(Color.LightGreen);
					break;
				case 4:
					gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
						.get_Item(1)
						.get_Style()
						.set_BackColor(Color.LightPink);
					break;
				}
			}
			else if ((columnIndex == 3) & (gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(3)
				.get_Value()
				.ToString() == "1"))
			{
				gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
					.get_Item(3)
					.set_Value((object)"2");
			}
			else if ((columnIndex == 3) & (gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(3)
				.get_Value()
				.ToString() == "2"))
			{
				gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
					.get_Item(3)
					.set_Value((object)"3");
			}
			else if ((columnIndex == 3) & (gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
				.get_Item(3)
				.get_Value()
				.ToString() == "3"))
			{
				gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
					.get_Item(3)
					.set_Value((object)"4");
			}
			else
			{
				gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
					.get_Item(columnIndex)
					.set_Value((object)"");
				if (columnIndex == 2)
				{
					AOTAData.StarValid[GridTimesSelectedRow] = true;
				}
				gridFrameTimes.get_Rows().get_Item(GridTimesSelectedRow).get_Cells()
					.get_Item(1)
					.get_Style()
					.set_BackColor(SystemColors.Window);
			}
		}

		private void updnIntegrateInterval_ValueChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbNumInteg).get_SelectedIndex() == 0)
			{
				((Control)cmdCheckMeasures).set_Enabled(true);
				((Control)cmdPlotFolded).set_Enabled(false);
				return;
			}
			((Control)cmdCheckMeasures).set_Enabled(false);
			lstDuplicates2.get_Items().Clear();
			((Control)cmdPlotFolded).set_Enabled(true);
			PlotFolded();
		}

		private void cmdCheckMeasures_Click(object sender, EventArgs e)
		{
			CheckMeasures();
		}

		private void CheckMeasures()
		{
			lstDuplicates2.get_Items().Clear();
			for (int i = 1; i < AOTAData.StarCount; i++)
			{
				if (AOTAData.Star[i] != AOTAData.Star[i - 1])
				{
					continue;
				}
				if (AOTAData.Comp1Used)
				{
					if (AOTAData.Comp1[i] == AOTAData.Comp1[i - 1])
					{
						lstDuplicates2.get_Items().Add((object)AOTAData.FrameID[i]);
					}
				}
				else
				{
					lstDuplicates2.get_Items().Add((object)AOTAData.FrameID[i]);
				}
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

		private void IntegrityCheck_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Form)DisplayOCR).Close();
			if (AOTAData.IntegrationFromLightCurve)
			{
				((Form)LightData.LightCurveForm).set_WindowState((FormWindowState)0);
			}
			else if (AOTAData.PlotForm != null)
			{
				((Form)AOTAData.PlotForm).set_WindowState((FormWindowState)0);
			}
			((Component)this).Dispose();
		}

		private void cmdInsertDroppedFrames_Click(object sender, EventArgs e)
		{
			for (int num = gridFrameTimes.get_RowCount() - 1; num >= 0; num--)
			{
				if (!((gridFrameTimes.get_Rows().get_Item(num).get_Cells()
					.get_Item(3)
					.get_Value()
					.ToString() != "") & (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
					.get_Item(4)
					.get_Value()
					.ToString() == "D")))
				{
					if (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(3)
						.get_Value()
						.ToString() == "1")
					{
						AOTAData.InsertDroppedFrame(num, 1);
					}
					if (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(3)
						.get_Value()
						.ToString() == "2")
					{
						AOTAData.InsertDroppedFrame(num, 2);
					}
					if (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(3)
						.get_Value()
						.ToString() == "3")
					{
						AOTAData.InsertDroppedFrame(num, 3);
					}
					if (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(3)
						.get_Value()
						.ToString() == "4")
					{
						AOTAData.InsertDroppedFrame(num, 4);
					}
					if (gridFrameTimes.get_Rows().get_Item(num).get_Cells()
						.get_Item(4)
						.get_Value()
						.ToString() == "D")
					{
						AOTAData.DeleteFrame(num);
					}
				}
			}
			LoadGridTable();
			PlotFolded();
		}

		private void cmdPlotFolded_Click(object sender, EventArgs e)
		{
			PlotFolded();
		}

		internal void PlotFolded()
		{
			int num = 5;
			float num2 = 0f;
			float y = 0f;
			float num3 = 0.1f;
			string text = "";
			Pen pen = new Pen(Color.DarkBlue, 2f);
			Pen pen2 = new Pen(Color.LightBlue, 1f);
			Pen pen3 = new Pen(Color.Pink, 2f);
			Pen pen4 = new Pen(Color.Pink, 1f);
			pen4.DashPattern = new float[2] { 3f, 1.5f };
			Pen pen5 = new Pen(Color.Green, 1f);
			pen5.DashStyle = DashStyle.Dot;
			Pen pen6 = new Pen(Color.Brown, 1f);
			Pen pen7 = new Pen(Color.White, 3f);
			Pen pen8 = new Pen(Color.Black, 1f);
			_ = Brushes.Gold;
			Brush blanchedAlmond = Brushes.BlanchedAlmond;
			Font font = new Font("Times New Roman", 9f, FontStyle.Bold);
			if (((ListControl)cmbNumInteg).get_SelectedIndex() < 0)
			{
				((ListControl)cmbNumInteg).set_SelectedIndex(0);
			}
			int num4 = int.Parse(cmbNumInteg.get_Items().get_Item(((ListControl)cmbNumInteg).get_SelectedIndex()).ToString());
			int num5 = (int)updnFirstFrame.get_Value();
			int num6 = 512;
			FoldHeight = 60;
			Xscale = 4f;
			if (num4 > 4)
			{
				Xscale = 2f;
			}
			if (num4 > 16)
			{
				Xscale = 1f;
			}
			NumPlotPoints = (int)((float)num6 / Xscale);
			int num7 = NumPlotPoints / num4;
			if ((num4 > 4) & chkFoldShort.get_Checked())
			{
				num4--;
				NumPlotPoints -= num7;
			}
			int num8;
			((Control)picFold).set_Width(num8 = (int)(((float)NumPlotPoints + (float)(num * 4) / Xscale) * Xscale));
			num6 = num8;
			((Control)panelPlot).set_Width(num6 + 24);
			int num9 = (int)Math.Ceiling((float)AOTAData.StarCount / (float)(NumPlotPoints + 1));
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				if (num3 < AOTAData.Star[i])
				{
					num3 = AOTAData.Star[i];
				}
			}
			float num10 = (float)((double)((float)FoldHeight / num3) / 1.1);
			((Control)picFold).set_Height(FoldHeight * num9 + 30);
			if (((Control)picFold).get_Height() < ((Control)panelPlot).get_Height() - 30)
			{
				((Control)picFold).set_Height(((Control)panelPlot).get_Height() - 30);
			}
			int height = ((Control)picFold).get_Height();
			Bitmap image = new Bitmap(num6, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			for (int j = 0; j < num9; j++)
			{
				float num11 = FoldHeight * (j + 1);
				for (int k = 0; (float)k < (float)NumPlotPoints + (float)(num * 4) / Xscale; k++)
				{
					num2 = (float)k * Xscale;
					if (j == 0)
					{
						if (k == NumPlotPoints + 1)
						{
							graphics.FillRectangle(blanchedAlmond, num2, 0f, (float)num6 - num2, height);
						}
						if ((k - num5) % num4 == 0)
						{
							graphics.DrawLine(pen5, num2, 0f, num2, height);
						}
						if ((float)k % (5f * Xscale / 4f) == 0f)
						{
							graphics.DrawLine(pen8, num2, height, num2, height - 9);
						}
						if ((float)k * Xscale / 4f % 10f == 0f)
						{
							graphics.DrawLine(pen8, num2, height, num2, height - 13);
							text = k.ToString().PadLeft(2);
							graphics.DrawString(text, font, Brushes.Black, num2 + 0.5f * Xscale - graphics.MeasureString(text, font).Width / 2f, height - 28);
						}
					}
					int num12 = j * NumPlotPoints + k;
					if (num12 >= AOTAData.StarCount)
					{
						break;
					}
					float num13 = num11 - num10 * AOTAData.Star[num12];
					if (k > 0)
					{
						if (!AOTAData.StarValid[num12 - 1] | !AOTAData.StarValid[num12])
						{
							graphics.DrawLine(pen4, num2, y, num2, num13);
						}
						else
						{
							graphics.DrawLine(pen2, num2, y, num2, num13);
						}
					}
					if (!AOTAData.StarValid[num12])
					{
						graphics.DrawLine(pen3, num2, num13, num2 + Xscale, num13);
					}
					else
					{
						graphics.DrawLine(pen, num2, num13, num2 + Xscale, num13);
					}
					y = num13;
				}
				graphics.DrawLine(pen6, 0f, num11, num6, num11);
				graphics.DrawLine(pen7, 0f, num11 + 2f, num6, num11 + 2f);
				text = AOTAData.FrameID[j * NumPlotPoints].ToString().PadLeft(2);
				graphics.DrawString(text, font, Brushes.Black, 5f, num11 - 14f);
			}
			picFold.set_Image((Image)image);
			graphics.Dispose();
		}

		private void updnFirstFrame_ValueChanged(object sender, EventArgs e)
		{
			PlotFolded();
		}

		private void cmbNumInteg_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbNumInteg).get_SelectedIndex() == 0)
			{
				((Control)cmdCheckMeasures).set_Enabled(true);
			}
			else
			{
				lstDuplicates2.get_Items().Clear();
				((Control)cmdCheckMeasures).set_Enabled(false);
			}
			PlotFolded();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AOTA Integrity");
		}

		private void picFold_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_008e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0098: Invalid comparison between Unknown and I4
			int x = e.get_X();
			int num = (int)Math.Floor((double)e.get_Y() / (double)FoldHeight);
			int num2 = (int)Math.Floor((float)x / Xscale);
			int num3 = NumPlotPoints * num + num2;
			string text = ((num3 >= AOTAData.StarCount) ? "---" : AOTAData.FrameID[num3].ToString());
			GetToolTip = !GetToolTip;
			if (GetToolTip)
			{
				toolTip1.SetToolTip((Control)(object)picFold, text);
			}
			toolTip1.set_ShowAlways(true);
			if ((int)e.get_Button() == 1048576 && num3 < AOTAData.StarCount)
			{
				gridFrameTimes.set_CurrentCell(gridFrameTimes.get_Rows().get_Item(num3).get_Cells()
					.get_Item(1));
			}
		}

		private void chkFoldShort_CheckedChanged(object sender, EventArgs e)
		{
			PlotFolded();
		}

		private void cmdSetTimesDropped_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDropped.get_Items().get_Count(); i++)
			{
				string text = lstDropped.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(3)
							.set_Value((object)"X");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(Color.LightGreen);
						break;
					}
				}
			}
		}

		private void cmdClearTimesDropped_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDropped.get_Items().get_Count(); i++)
			{
				string text = lstDropped.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(3)
							.set_Value((object)"");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(SystemColors.Window);
						break;
					}
				}
			}
		}

		private void cmdSetTimesDuplicated_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDuplicates.get_Items().get_Count(); i++)
			{
				string text = lstDuplicates.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(4)
							.set_Value((object)"X");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(Color.LightPink);
						break;
					}
				}
			}
		}

		private void cmdClearTimesDuplicated_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDuplicates.get_Items().get_Count(); i++)
			{
				string text = lstDuplicates.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(4)
							.set_Value((object)"");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(SystemColors.Window);
						break;
					}
				}
			}
		}

		private void cmdSetMeasuresDuplicated_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDuplicates2.get_Items().get_Count(); i++)
			{
				string text = lstDuplicates2.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(4)
							.set_Value((object)"X");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(Color.LightPink);
						break;
					}
				}
			}
		}

		private void cmdClearMeasuresDuplicated_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < lstDuplicates2.get_Items().get_Count(); i++)
			{
				string text = lstDuplicates2.get_Items().get_Item(i).ToString();
				for (int j = 1; j < AOTAData.StarCount; j++)
				{
					if (((int)AOTAData.FrameID[j]).ToString() == text)
					{
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(4)
							.set_Value((object)"");
						gridFrameTimes.get_Rows().get_Item(j).get_Cells()
							.get_Item(1)
							.get_Style()
							.set_BackColor(SystemColors.Window);
						break;
					}
				}
			}
		}

		private void ClearAllChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < AOTAData.StarCount; i++)
			{
				DataGridViewCell obj = gridFrameTimes.get_Rows().get_Item(i).get_Cells()
					.get_Item(2);
				DataGridViewCell obj2 = gridFrameTimes.get_Rows().get_Item(i).get_Cells()
					.get_Item(3);
				object obj3;
				gridFrameTimes.get_Rows().get_Item(i).get_Cells()
					.get_Item(4)
					.set_Value(obj3 = "");
				object value;
				obj2.set_Value(value = obj3);
				obj.set_Value(value);
				gridFrameTimes.get_Rows().get_Item(i).get_Cells()
					.get_Item(1)
					.get_Style()
					.set_BackColor(SystemColors.Window);
			}
		}

		private void cmdExit_Click_1(object sender, EventArgs e)
		{
			for (int i = 0; i < 5; i++)
			{
				AOTAData.FrameTimeCorrnsD[i] = (AOTAData.FrameTimeCorrnsR[i] = 0.0);
			}
			((Form)this).Close();
		}

		private void CorrectOCR()
		{
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			int StartOfValid = 0;
			int StartOfValid2 = 0;
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			bool flag = false;
			double FrameDiff = 0.0;
			double FrameDiff2 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			int num6 = 0;
			string text = "";
			string text2 = "";
			int num7 = 0;
			string text3 = "";
			IsCorrectingOCR = true;
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			if (GetNext20ValidTimeIntervals(0, out StartOfValid, out FrameDiff, out var FrameRateVariation_Secs))
			{
				num = StartOfValid + 20;
				int num8 = 0;
				int num9 = 0;
				do
				{
					if (Math.Abs(AOTAData.FrameTimeSecs[num + 1] - AOTAData.FrameTimeSecs[num] - FrameDiff) > 0.002)
					{
						num3 = 0;
						flag = false;
						do
						{
							if (GetNext20ValidTimeIntervals(num + 1 + num3, out StartOfValid2, out FrameDiff2, out FrameRateVariation_Secs))
							{
								if (FrameDiff2 == -1.0)
								{
									break;
								}
								if (Math.Abs(FrameDiff2 - FrameDiff) < 0.001)
								{
									num4 = AOTAData.FrameTimeSecs[StartOfValid2] - AOTAData.FrameTimeSecs[num] - (double)(StartOfValid2 - num) * FrameDiff;
									num5 = num4 / FrameDiff;
									num6 = (double.IsNaN(num5) ? 20 : Convert.ToInt32(num5));
									if (Math.Abs(num6) > 0 && Math.Abs(num6) < 10 && Math.Abs(num5 - (double)num6) < 0.01)
									{
										string text4 = string.Format("{0} frames between frames {1,0:f1} and {2,0:f1}", Math.Abs(num6), AOTAData.FrameID[num], AOTAData.FrameID[StartOfValid2]);
										if (num6 > 0)
										{
											text2 = text2 + text4 + "\r\n";
											num8++;
										}
										else
										{
											text = text + text4 + "\r\n";
											num9++;
										}
										num = StartOfValid2 + 18;
										FrameDiff = FrameDiff2;
										flag = true;
										if (num8 > 10 || num9 > 10)
										{
											MessageBox.Show("The input file has " + num9 + " dropped frames\r\nand " + num8 + " duplicated frames.\r\n\r\nThis is too many problems with the video time base for them to be corrected.\r\n\r\nAOTA will continue, and interpret the light curve without correcting the time base for errors (including\r\nwhether there are dropped or duplicated frames). The event times derived by AOTA will necessarily be unreliable.\r\n\r\nYou need to review your video to determine whether the problem is with the video, or with its measurement.", "Too many errors", (MessageBoxButtons)0, (MessageBoxIcon)16);
											((Control)this).set_Cursor(Cursors.get_Default());
											return;
										}
									}
									else if (Math.Abs(num4) < 0.005)
									{
										num2 = num + 1;
										((Control)lblOCRprogress).set_Text(num2.ToString());
										Application.DoEvents();
										do
										{
											if (Math.Abs(AOTAData.FrameTimeSecs[num2] - AOTAData.FrameTimeSecs[num2 - 1] - FrameDiff) > 0.002)
											{
												AOTAData.FrameTimeSecs[num2] = AOTAData.FrameTimeSecs[num2 - 1] + FrameDiff;
												num7++;
												text3 = text3 + AOTAData.FrameID[num2] + "  ";
											}
											num2++;
										}
										while (num2 < StartOfValid2);
										num = StartOfValid2 + 18;
										FrameDiff = FrameDiff2;
										flag = true;
									}
								}
							}
							if (flag || StartOfValid2 < num)
							{
								break;
							}
							num3 = StartOfValid2 - num + 20;
						}
						while (num + 1 + num3 < AOTAData.StarCount - 1);
						if (FrameDiff2 == -1.0)
						{
							break;
						}
					}
					num++;
				}
				while (num < AOTAData.StarCount - 1);
			}
			((Control)lblOCRprogress).set_Text("OCR progress");
			LoadGridTable();
			PlotFolded();
			((Control)this).set_Cursor(Cursors.get_Default());
			OCRmessage = "";
			if (((text2.Length > 0) | (text.Length > 0)) || num7 > 0)
			{
				if (num7 > 0)
				{
					OCRmessage = OCRmessage + "The number of OCR'd times that have been corrected is: " + num7 + "\r\n\r\nat frames " + text3 + "\r\n\r\n";
				}
				if ((text2.Length > 0) | (text.Length > 0))
				{
					if (text2.Length > 0)
					{
						OCRmessage = OCRmessage + "Possible dropped frames have been detected as:\r\n" + text2 + "\r\n";
					}
					if (text.Length > 0)
					{
						OCRmessage = OCRmessage + "Possible duplicated frames have been detected as:\r\n" + text + "\r\n";
					}
					OCRmessage += "When the differences between the frames is 10's of frames, the 'possible' dropped or duplicated frames is no more than a change in the frame rate - and requires no correction. 'Real' duplicated and dropped frames should be identified on the form. Take care to ensure you review the frames on either side to ensure all necessary corrections are made.";
				}
				ShowOCRmessage();
			}
			IsCorrectingOCR = false;
		}

		private bool GetNext20ValidTimeIntervals(int StartFrame, out int StartOfValid, out double FrameDiff, out double FrameRateVariation_Secs)
		{
			FrameDiff = 0.0;
			FrameRateVariation_Secs = 0.0;
			double num = 0.0;
			StartOfValid = -1;
			bool flag = false;
			int num2 = 20;
			int num3 = 100;
			if (AOTAData.StarCount < 100)
			{
				num3 = AOTAData.StarCount;
			}
			for (int i = StartFrame; i < num3 - 1; i++)
			{
				if (i + num2 >= AOTAData.StarCount)
				{
					flag = false;
					FrameDiff = -1.0;
					return flag;
				}
				FrameDiff = (AOTAData.FrameTimeSecs[i + num2] - AOTAData.FrameTimeSecs[i]) / (double)num2;
				flag = true;
				for (int j = 0; j < num2; j++)
				{
					num = Math.Abs(FrameDiff - (AOTAData.FrameTimeSecs[i + j + 1] - AOTAData.FrameTimeSecs[i + j]));
					if (num > FrameRateVariation_Secs)
					{
						FrameRateVariation_Secs = num;
					}
					if (num > 0.1 * FrameDiff)
					{
						flag = false;
						FrameDiff = 0.0;
						break;
					}
				}
				if (flag)
				{
					StartOfValid = i;
					break;
				}
			}
			return flag;
		}

		private void viewCorrectionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowOCRmessage();
		}

		internal void ShowOCRmessage()
		{
			try
			{
				((Control)DisplayOCR).Show();
			}
			catch
			{
				DisplayOCR = new DisplayData();
				((Control)DisplayOCR).Show();
			}
			((Control)DisplayOCR).Show();
			((ToolStripItem)DisplayOCR.cmdCancel).set_Visible(false);
			((ToolStripItem)DisplayOCR.cmdOK).set_Visible(false);
			((Control)DisplayOCR).set_Text("OCR of Time stamps - issues identified : Occult v." + Utilities.OccultVersion_Short);
			((Control)DisplayOCR.txtBox).set_Text(OCRmessage);
			((Form)DisplayOCR).set_TopMost(true);
			if (((Control)this).get_Top() > 30)
			{
				((Control)DisplayOCR).set_Top(((Control)this).get_Top() - 30);
			}
			else
			{
				((Control)DisplayOCR).set_Top(((Control)this).get_Top() + ((Control)this).get_Height() - 30);
			}
			if (((Control)this).get_Left() > 30)
			{
				((Control)DisplayOCR).set_Left(((Control)this).get_Left() - 30);
			}
			else
			{
				((Control)DisplayOCR).set_Left(((Control)this).get_Left() + ((Control)this).get_Width() - 30);
			}
			((TextBoxBase)DisplayOCR.txtBox).Select(0, 0);
		}

		private void checkOCRForErrorsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CorrectOCR();
		}

		private void autoCheckToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.OCR_Automatic = !Settings.Default.OCR_Automatic;
			autoCheckToolStripMenuItem.set_Checked(Settings.Default.OCR_Automatic);
		}

		private void chkSecDecPlaces_CheckedChanged(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			LoadGridTable();
			((Control)this).set_Cursor(((Control)this).get_DefaultCursor());
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
			//IL_000b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0011: Expected O, but got Unknown
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0017: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
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
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b3: Expected O, but got Unknown
			//IL_01b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01be: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01da: Expected O, but got Unknown
			//IL_01db: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e5: Expected O, but got Unknown
			//IL_01e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f0: Expected O, but got Unknown
			//IL_01f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fb: Expected O, but got Unknown
			//IL_01fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0206: Expected O, but got Unknown
			//IL_0207: Unknown result type (might be due to invalid IL or missing references)
			//IL_0211: Expected O, but got Unknown
			//IL_0212: Unknown result type (might be due to invalid IL or missing references)
			//IL_021c: Expected O, but got Unknown
			//IL_021d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0227: Expected O, but got Unknown
			//IL_0228: Unknown result type (might be due to invalid IL or missing references)
			//IL_0232: Expected O, but got Unknown
			//IL_0233: Unknown result type (might be due to invalid IL or missing references)
			//IL_023d: Expected O, but got Unknown
			//IL_023e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0248: Expected O, but got Unknown
			//IL_0249: Unknown result type (might be due to invalid IL or missing references)
			//IL_0253: Expected O, but got Unknown
			//IL_0254: Unknown result type (might be due to invalid IL or missing references)
			//IL_025e: Expected O, but got Unknown
			//IL_025f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0269: Expected O, but got Unknown
			//IL_026a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0274: Expected O, but got Unknown
			//IL_0275: Unknown result type (might be due to invalid IL or missing references)
			//IL_027f: Expected O, but got Unknown
			//IL_0280: Unknown result type (might be due to invalid IL or missing references)
			//IL_028a: Expected O, but got Unknown
			//IL_028b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0295: Expected O, but got Unknown
			//IL_0296: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a0: Expected O, but got Unknown
			//IL_04b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c0: Expected O, but got Unknown
			//IL_04cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d7: Expected O, but got Unknown
			//IL_065c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0666: Expected O, but got Unknown
			//IL_06c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_06cc: Expected O, but got Unknown
			//IL_0d2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d39: Expected O, but got Unknown
			//IL_13f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_13fe: Expected O, but got Unknown
			//IL_1e19: Unknown result type (might be due to invalid IL or missing references)
			//IL_1e23: Expected O, but got Unknown
			//IL_212f: Unknown result type (might be due to invalid IL or missing references)
			//IL_2139: Expected O, but got Unknown
			components = new Container();
			DataGridViewCellStyle val = new DataGridViewCellStyle();
			DataGridViewCellStyle val2 = new DataGridViewCellStyle();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(IntegrityCheck));
			gridFrameTimes = new DataGridView();
			Frame = new DataGridViewTextBoxColumn();
			UTC = new DataGridViewTextBoxColumn();
			Valid = new DataGridViewTextBoxColumn();
			Dropped = new DataGridViewTextBoxColumn();
			Delete = new DataGridViewTextBoxColumn();
			lstDropped = new ListBox();
			lstDuplicates = new ListBox();
			label1 = new Label();
			label2 = new Label();
			txtFrameID = new TextBox();
			txtMins = new TextBox();
			txtSecs = new TextBox();
			txtHrs = new TextBox();
			cmdSetTime = new Button();
			label44 = new Label();
			label43 = new Label();
			label42 = new Label();
			label7 = new Label();
			cmdCheckDroppedDuplicates = new Button();
			cmdCheckMeasures = new Button();
			cmdPlotFolded = new Button();
			lstDuplicates2 = new ListBox();
			label3 = new Label();
			cmdInsertDroppedFrames = new Button();
			label4 = new Label();
			updnFirstFrame = new NumericUpDown();
			label5 = new Label();
			cmbNumInteg = new ComboBox();
			menuStrip1 = new MenuStrip();
			viewOCRCorrectionsToolStripMenuItem = new ToolStripMenuItem();
			autoCheckToolStripMenuItem = new ToolStripMenuItem();
			checkOCRForErrorsToolStripMenuItem = new ToolStripMenuItem();
			viewCorrectionsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			label6 = new Label();
			panelPlot = new Panel();
			picFold = new PictureBox();
			toolTip1 = new ToolTip(components);
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			chkFoldShort = new CheckBox();
			cmdSetTimesDropped = new Button();
			cmdClearTimesDropped = new Button();
			cmdClearTimesDuplicated = new Button();
			cmdSetTimesDuplicated = new Button();
			cmdClearMeasuresDuplicated = new Button();
			cmdSetMeasuresDuplicated = new Button();
			ClearAllChecks = new Button();
			panel1 = new Panel();
			panel2 = new Panel();
			cmdExit = new Button();
			lblOCRprogress = new Label();
			lblFrameRate = new Label();
			chkSecDecPlaces = new CheckBox();
			((ISupportInitialize)gridFrameTimes).BeginInit();
			((ISupportInitialize)updnFirstFrame).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelPlot).SuspendLayout();
			((ISupportInitialize)picFold).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)this).SuspendLayout();
			gridFrameTimes.set_AllowUserToAddRows(false);
			gridFrameTimes.set_AllowUserToDeleteRows(false);
			gridFrameTimes.set_AllowUserToResizeColumns(false);
			gridFrameTimes.set_AllowUserToResizeRows(false);
			gridFrameTimes.set_BorderStyle((BorderStyle)2);
			val.set_Alignment((DataGridViewContentAlignment)16);
			val.set_BackColor(SystemColors.Control);
			val.set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			val.set_ForeColor(SystemColors.WindowText);
			val.set_SelectionBackColor(SystemColors.Highlight);
			val.set_SelectionForeColor(SystemColors.HighlightText);
			val.set_WrapMode((DataGridViewTriState)1);
			gridFrameTimes.set_ColumnHeadersDefaultCellStyle(val);
			gridFrameTimes.set_ColumnHeadersHeightSizeMode((DataGridViewColumnHeadersHeightSizeMode)2);
			gridFrameTimes.get_Columns().AddRange((DataGridViewColumn[])(object)new DataGridViewColumn[5]
			{
				(DataGridViewColumn)Frame,
				(DataGridViewColumn)UTC,
				(DataGridViewColumn)Valid,
				(DataGridViewColumn)Dropped,
				(DataGridViewColumn)Delete
			});
			val2.set_Alignment((DataGridViewContentAlignment)16);
			val2.set_BackColor(SystemColors.Window);
			val2.set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			val2.set_ForeColor(SystemColors.ControlText);
			val2.set_SelectionBackColor(SystemColors.Highlight);
			val2.set_SelectionForeColor(SystemColors.HighlightText);
			val2.set_WrapMode((DataGridViewTriState)2);
			gridFrameTimes.set_DefaultCellStyle(val2);
			((Control)gridFrameTimes).set_Location(new Point(5, 78));
			gridFrameTimes.set_MultiSelect(false);
			((Control)gridFrameTimes).set_Name("gridFrameTimes");
			gridFrameTimes.set_ReadOnly(true);
			gridFrameTimes.set_ScrollBars((ScrollBars)2);
			((Control)gridFrameTimes).set_Size(new Size(304, 395));
			((Control)gridFrameTimes).set_TabIndex(1);
			gridFrameTimes.add_RowEnter(new DataGridViewCellEventHandler(gridFrameTimes_RowEnter));
			((Control)gridFrameTimes).add_MouseDoubleClick(new MouseEventHandler(gridFrameTimes_MouseDoubleClick));
			((DataGridViewColumn)Frame).set_HeaderText("Frame #");
			((DataGridViewColumn)Frame).set_Name("Frame");
			((DataGridViewBand)Frame).set_ReadOnly(true);
			Frame.set_SortMode((DataGridViewColumnSortMode)0);
			((DataGridViewColumn)Frame).set_Width(50);
			((DataGridViewColumn)UTC).set_HeaderText("UTC at start");
			((DataGridViewColumn)UTC).set_Name("UTC");
			((DataGridViewBand)UTC).set_ReadOnly(true);
			UTC.set_SortMode((DataGridViewColumnSortMode)0);
			((DataGridViewColumn)UTC).set_Width(80);
			((DataGridViewColumn)Valid).set_HeaderText("Invalid");
			((DataGridViewColumn)Valid).set_Name("Valid");
			((DataGridViewBand)Valid).set_ReadOnly(true);
			((DataGridViewColumn)Valid).set_Width(40);
			((DataGridViewColumn)Dropped).set_HeaderText("Add");
			((DataGridViewColumn)Dropped).set_Name("Dropped");
			((DataGridViewBand)Dropped).set_ReadOnly(true);
			((DataGridViewColumn)Dropped).set_Width(32);
			((DataGridViewColumn)Delete).set_HeaderText("Delete");
			((DataGridViewColumn)Delete).set_Name("Delete");
			((DataGridViewBand)Delete).set_ReadOnly(true);
			((ListControl)lstDropped).set_FormattingEnabled(true);
			((Control)lstDropped).set_Location(new Point(15, 50));
			((Control)lstDropped).set_Name("lstDropped");
			((Control)lstDropped).set_Size(new Size(57, 108));
			((Control)lstDropped).set_TabIndex(3);
			((Control)lstDropped).add_MouseDoubleClick(new MouseEventHandler(lstDropped_MouseDoubleClick));
			((ListControl)lstDuplicates).set_FormattingEnabled(true);
			((Control)lstDuplicates).set_Location(new Point(4, 48));
			((Control)lstDuplicates).set_Name("lstDuplicates");
			((Control)lstDuplicates).set_Size(new Size(57, 108));
			((Control)lstDuplicates).set_TabIndex(4);
			((Control)lstDuplicates).add_MouseDoubleClick(new MouseEventHandler(lstDuplicates_MouseDoubleClick));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(5, 3));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(117, 26));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Possible duplicated\r\nimages");
			label1.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(16, 3));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(96, 26));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Possible missed\r\nimages");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtFrameID).set_Location(new Point(11, 490));
			((Control)txtFrameID).set_Name("txtFrameID");
			((TextBoxBase)txtFrameID).set_ReadOnly(true);
			((Control)txtFrameID).set_Size(new Size(55, 20));
			((Control)txtFrameID).set_TabIndex(11);
			((Control)txtMins).set_Location(new Point(115, 490));
			((Control)txtMins).set_Name("txtMins");
			((Control)txtMins).set_Size(new Size(26, 20));
			((Control)txtMins).set_TabIndex(15);
			((Control)txtSecs).set_Location(new Point(149, 490));
			((Control)txtSecs).set_Name("txtSecs");
			((Control)txtSecs).set_Size(new Size(49, 20));
			((Control)txtSecs).set_TabIndex(17);
			((Control)txtHrs).set_Location(new Point(81, 490));
			((Control)txtHrs).set_Name("txtHrs");
			((Control)txtHrs).set_Size(new Size(26, 20));
			((Control)txtHrs).set_TabIndex(13);
			((Control)cmdSetTime).set_BackColor(Color.MediumSpringGreen);
			((Control)cmdSetTime).set_Location(new Point(12, 512));
			((Control)cmdSetTime).set_Name("cmdSetTime");
			((Control)cmdSetTime).set_Size(new Size(187, 37));
			((Control)cmdSetTime).set_TabIndex(18);
			((Control)cmdSetTime).set_Text("Set this time for the selected frame");
			((ButtonBase)cmdSetTime).set_UseVisualStyleBackColor(false);
			((Control)cmdSetTime).add_Click((EventHandler)cmdSetTime_Click);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(13, 475));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(50, 13));
			((Control)label44).set_TabIndex(10);
			((Control)label44).set_Text("Frame ID");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(160, 475));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(26, 13));
			((Control)label43).set_TabIndex(16);
			((Control)label43).set_Text("Sec");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(116, 475));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(24, 13));
			((Control)label42).set_TabIndex(14);
			((Control)label42).set_Text("Min");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(85, 475));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(18, 13));
			((Control)label7).set_TabIndex(12);
			((Control)label7).set_Text("Hr");
			((Control)cmdCheckDroppedDuplicates).set_BackColor(Color.PaleGreen);
			((Control)cmdCheckDroppedDuplicates).set_Location(new Point(330, 38));
			((Control)cmdCheckDroppedDuplicates).set_Name("cmdCheckDroppedDuplicates");
			((Control)cmdCheckDroppedDuplicates).set_Size(new Size(104, 24));
			((Control)cmdCheckDroppedDuplicates).set_TabIndex(19);
			((Control)cmdCheckDroppedDuplicates).set_Text("Check Times");
			((ButtonBase)cmdCheckDroppedDuplicates).set_UseVisualStyleBackColor(false);
			((Control)cmdCheckDroppedDuplicates).add_Click((EventHandler)cmdCheckDroppedDuplicates_Click);
			((Control)cmdCheckMeasures).set_BackColor(Color.Wheat);
			((Control)cmdCheckMeasures).set_Location(new Point(330, 68));
			((Control)cmdCheckMeasures).set_Name("cmdCheckMeasures");
			((Control)cmdCheckMeasures).set_Size(new Size(104, 24));
			((Control)cmdCheckMeasures).set_TabIndex(21);
			((Control)cmdCheckMeasures).set_Text("Check Measures");
			((ButtonBase)cmdCheckMeasures).set_UseVisualStyleBackColor(false);
			((Control)cmdCheckMeasures).add_Click((EventHandler)cmdCheckMeasures_Click);
			((Control)cmdPlotFolded).set_BackColor(Color.Aquamarine);
			((Control)cmdPlotFolded).set_Location(new Point(690, 507));
			((Control)cmdPlotFolded).set_Name("cmdPlotFolded");
			((Control)cmdPlotFolded).set_Size(new Size(103, 36));
			((Control)cmdPlotFolded).set_TabIndex(22);
			((Control)cmdPlotFolded).set_Text("Plot folded\r\nlight curve");
			((ButtonBase)cmdPlotFolded).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotFolded).add_Click((EventHandler)cmdPlotFolded_Click);
			((ListControl)lstDuplicates2).set_FormattingEnabled(true);
			((Control)lstDuplicates2).set_Location(new Point(68, 48));
			((Control)lstDuplicates2).set_Name("lstDuplicates2");
			((Control)lstDuplicates2).set_Size(new Size(57, 108));
			((Control)lstDuplicates2).set_TabIndex(23);
			((Control)lstDuplicates2).add_MouseDoubleClick(new MouseEventHandler(lstDuplicates2_MouseDoubleClick));
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(499, 499));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(98, 13));
			((Control)label3).set_TabIndex(24);
			((Control)label3).set_Text("# frames integrated");
			((Control)cmdInsertDroppedFrames).set_BackColor(Color.LemonChiffon);
			((Control)cmdInsertDroppedFrames).set_Location(new Point(321, 505));
			((Control)cmdInsertDroppedFrames).set_Name("cmdInsertDroppedFrames");
			((Control)cmdInsertDroppedFrames).set_Size(new Size(122, 40));
			((Control)cmdInsertDroppedFrames).set_TabIndex(25);
			((Control)cmdInsertDroppedFrames).set_Text("Insert/Delete images\r\n- and re-load");
			((ButtonBase)cmdInsertDroppedFrames).set_UseVisualStyleBackColor(false);
			((Control)cmdInsertDroppedFrames).add_Click((EventHandler)cmdInsertDroppedFrames_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(608, 499));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(65, 13));
			((Control)label4).set_TabIndex(27);
			((Control)label4).set_Text("Starting at #");
			((Control)updnFirstFrame).set_Location(new Point(615, 515));
			updnFirstFrame.set_Maximum(new decimal(new int[4] { 256, 0, 0, 0 }));
			((Control)updnFirstFrame).set_Name("updnFirstFrame");
			((Control)updnFirstFrame).set_Size(new Size(47, 20));
			((Control)updnFirstFrame).set_TabIndex(26);
			updnFirstFrame.add_ValueChanged((EventHandler)updnFirstFrame_ValueChanged);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(6, 27));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(287, 26));
			((Control)label5).set_TabIndex(28);
			((Control)label5).set_Text("Double-click in column to set a frame Invalid, to Add one or\r\nmore frames IMMEDIATELY before, or to Delete that frame.");
			((ListControl)cmbNumInteg).set_FormattingEnabled(true);
			cmbNumInteg.get_Items().AddRange(new object[9] { "1", "2", "4", "8", "16", "32", "64", "128", "256" });
			((Control)cmbNumInteg).set_Location(new Point(516, 515));
			((Control)cmbNumInteg).set_Name("cmbNumInteg");
			((Control)cmbNumInteg).set_Size(new Size(66, 21));
			((Control)cmbNumInteg).set_TabIndex(29);
			cmbNumInteg.add_SelectedIndexChanged((EventHandler)cmbNumInteg_SelectedIndexChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)viewOCRCorrectionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1012, 24));
			((Control)menuStrip1).set_TabIndex(30);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)viewOCRCorrectionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)autoCheckToolStripMenuItem,
				(ToolStripItem)checkOCRForErrorsToolStripMenuItem,
				(ToolStripItem)viewCorrectionsToolStripMenuItem
			});
			((ToolStripItem)viewOCRCorrectionsToolStripMenuItem).set_Name("viewOCRCorrectionsToolStripMenuItem");
			((ToolStripItem)viewOCRCorrectionsToolStripMenuItem).set_Size(new Size(146, 20));
			((ToolStripItem)viewOCRCorrectionsToolStripMenuItem).set_Text("OCR of Time Stamps...   ");
			autoCheckToolStripMenuItem.set_Checked(true);
			autoCheckToolStripMenuItem.set_CheckOnClick(true);
			autoCheckToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)autoCheckToolStripMenuItem).set_Name("autoCheckToolStripMenuItem");
			((ToolStripItem)autoCheckToolStripMenuItem).set_Size(new Size(188, 22));
			((ToolStripItem)autoCheckToolStripMenuItem).set_Text("Auto check OCR");
			((ToolStripItem)autoCheckToolStripMenuItem).add_Click((EventHandler)autoCheckToolStripMenuItem_Click);
			((ToolStripItem)checkOCRForErrorsToolStripMenuItem).set_Name("checkOCRForErrorsToolStripMenuItem");
			((ToolStripItem)checkOCRForErrorsToolStripMenuItem).set_Size(new Size(188, 22));
			((ToolStripItem)checkOCRForErrorsToolStripMenuItem).set_Text("Check OCR for errors");
			((ToolStripItem)checkOCRForErrorsToolStripMenuItem).add_Click((EventHandler)checkOCRForErrorsToolStripMenuItem_Click);
			((ToolStripItem)viewCorrectionsToolStripMenuItem).set_Name("viewCorrectionsToolStripMenuItem");
			((ToolStripItem)viewCorrectionsToolStripMenuItem).set_Size(new Size(188, 22));
			((ToolStripItem)viewCorrectionsToolStripMenuItem).set_Text("View OCR corrections");
			((ToolStripItem)viewCorrectionsToolStripMenuItem).add_Click((EventHandler)viewCorrectionsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help                ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(534, 29));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(370, 13));
			((Control)label6).set_TabIndex(32);
			((Control)label6).set_Text("Plot of light curve, folded at multiples of 2^x. Integration changes should align");
			((ScrollableControl)panelPlot).set_AutoScroll(true);
			panelPlot.set_BorderStyle((BorderStyle)2);
			((Control)panelPlot).get_Controls().Add((Control)(object)picFold);
			((Control)panelPlot).set_Location(new Point(454, 46));
			((Control)panelPlot).set_Name("panelPlot");
			((Control)panelPlot).set_Size(new Size(533, 426));
			((Control)panelPlot).set_TabIndex(33);
			((Control)picFold).set_BackColor(Color.White);
			picFold.set_BorderStyle((BorderStyle)1);
			((Control)picFold).set_Location(new Point(2, 2));
			((Control)picFold).set_Name("picFold");
			((Control)picFold).set_Size(new Size(504, 406));
			picFold.set_TabIndex(2);
			picFold.set_TabStop(false);
			((Control)picFold).add_MouseMove(new MouseEventHandler(picFold_MouseMove));
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(585, 478));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(268, 13));
			((Control)label8).set_TabIndex(34);
			((Control)label8).set_Text("To display a point - Click, then move mouse off the plot ");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(4, 33));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(41, 13));
			((Control)label9).set_TabIndex(35);
			((Control)label9).set_Text("- Times");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(65, 34));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(59, 13));
			((Control)label10).set_TabIndex(36);
			((Control)label10).set_Text("- Measures");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(23, 35));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(35, 13));
			((Control)label11).set_TabIndex(37);
			((Control)label11).set_Text("Times");
			((Control)chkFoldShort).set_AutoSize(true);
			((Control)chkFoldShort).set_Location(new Point(827, 517));
			((Control)chkFoldShort).set_Name("chkFoldShort");
			((Control)chkFoldShort).set_Size(new Size(87, 17));
			((Control)chkFoldShort).set_TabIndex(38);
			((Control)chkFoldShort).set_Text("Fold at 2^x-1");
			((ButtonBase)chkFoldShort).set_UseVisualStyleBackColor(true);
			chkFoldShort.add_CheckedChanged((EventHandler)chkFoldShort_CheckedChanged);
			((Control)cmdSetTimesDropped).set_BackColor(Color.PaleTurquoise);
			((Control)cmdSetTimesDropped).set_Location(new Point(79, 75));
			((Control)cmdSetTimesDropped).set_Name("cmdSetTimesDropped");
			((Control)cmdSetTimesDropped).set_Size(new Size(47, 22));
			((Control)cmdSetTimesDropped).set_TabIndex(39);
			((Control)cmdSetTimesDropped).set_Text("Set all");
			((ButtonBase)cmdSetTimesDropped).set_UseVisualStyleBackColor(false);
			((Control)cmdSetTimesDropped).add_Click((EventHandler)cmdSetTimesDropped_Click);
			((Control)cmdClearTimesDropped).set_BackColor(Color.Pink);
			((Control)cmdClearTimesDropped).set_Location(new Point(79, 103));
			((Control)cmdClearTimesDropped).set_Name("cmdClearTimesDropped");
			((Control)cmdClearTimesDropped).set_Size(new Size(47, 22));
			((Control)cmdClearTimesDropped).set_TabIndex(40);
			((Control)cmdClearTimesDropped).set_Text("Clr all");
			((ButtonBase)cmdClearTimesDropped).set_UseVisualStyleBackColor(false);
			((Control)cmdClearTimesDropped).add_Click((EventHandler)cmdClearTimesDropped_Click);
			((Control)cmdClearTimesDuplicated).set_BackColor(Color.Pink);
			((Control)cmdClearTimesDuplicated).set_Location(new Point(7, 190));
			((Control)cmdClearTimesDuplicated).set_Name("cmdClearTimesDuplicated");
			((Control)cmdClearTimesDuplicated).set_Size(new Size(47, 22));
			((Control)cmdClearTimesDuplicated).set_TabIndex(42);
			((Control)cmdClearTimesDuplicated).set_Text("Clr all");
			((ButtonBase)cmdClearTimesDuplicated).set_UseVisualStyleBackColor(false);
			((Control)cmdClearTimesDuplicated).add_Click((EventHandler)cmdClearTimesDuplicated_Click);
			((Control)cmdSetTimesDuplicated).set_BackColor(Color.PaleTurquoise);
			((Control)cmdSetTimesDuplicated).set_Location(new Point(7, 162));
			((Control)cmdSetTimesDuplicated).set_Name("cmdSetTimesDuplicated");
			((Control)cmdSetTimesDuplicated).set_Size(new Size(47, 22));
			((Control)cmdSetTimesDuplicated).set_TabIndex(41);
			((Control)cmdSetTimesDuplicated).set_Text("Set all");
			((ButtonBase)cmdSetTimesDuplicated).set_UseVisualStyleBackColor(false);
			((Control)cmdSetTimesDuplicated).add_Click((EventHandler)cmdSetTimesDuplicated_Click);
			((Control)cmdClearMeasuresDuplicated).set_BackColor(Color.Pink);
			((Control)cmdClearMeasuresDuplicated).set_Location(new Point(71, 190));
			((Control)cmdClearMeasuresDuplicated).set_Name("cmdClearMeasuresDuplicated");
			((Control)cmdClearMeasuresDuplicated).set_Size(new Size(47, 22));
			((Control)cmdClearMeasuresDuplicated).set_TabIndex(44);
			((Control)cmdClearMeasuresDuplicated).set_Text("Clr all");
			((ButtonBase)cmdClearMeasuresDuplicated).set_UseVisualStyleBackColor(false);
			((Control)cmdClearMeasuresDuplicated).add_Click((EventHandler)cmdClearMeasuresDuplicated_Click);
			((Control)cmdSetMeasuresDuplicated).set_BackColor(Color.PaleTurquoise);
			((Control)cmdSetMeasuresDuplicated).set_Location(new Point(71, 162));
			((Control)cmdSetMeasuresDuplicated).set_Name("cmdSetMeasuresDuplicated");
			((Control)cmdSetMeasuresDuplicated).set_Size(new Size(47, 22));
			((Control)cmdSetMeasuresDuplicated).set_TabIndex(43);
			((Control)cmdSetMeasuresDuplicated).set_Text("Set all");
			((ButtonBase)cmdSetMeasuresDuplicated).set_UseVisualStyleBackColor(false);
			((Control)cmdSetMeasuresDuplicated).add_Click((EventHandler)cmdSetMeasuresDuplicated_Click);
			((Control)ClearAllChecks).set_BackColor(Color.LightPink);
			((Control)ClearAllChecks).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)ClearAllChecks).set_Location(new Point(176, 55));
			((Control)ClearAllChecks).set_Name("ClearAllChecks");
			((Control)ClearAllChecks).set_Size(new Size(134, 22));
			((Control)ClearAllChecks).set_TabIndex(45);
			((Control)ClearAllChecks).set_Text("Clear all checks");
			((ButtonBase)ClearAllChecks).set_UseVisualStyleBackColor(false);
			((Control)ClearAllChecks).add_Click((EventHandler)ClearAllChecks_Click);
			((Control)panel1).set_BackColor(Color.Bisque);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)cmdClearMeasuresDuplicated);
			((Control)panel1).get_Controls().Add((Control)(object)cmdSetMeasuresDuplicated);
			((Control)panel1).get_Controls().Add((Control)(object)cmdClearTimesDuplicated);
			((Control)panel1).get_Controls().Add((Control)(object)cmdSetTimesDuplicated);
			((Control)panel1).get_Controls().Add((Control)(object)lstDuplicates2);
			((Control)panel1).get_Controls().Add((Control)(object)lstDuplicates);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(316, 279));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(132, 220));
			((Control)panel1).set_TabIndex(46);
			((Control)panel2).set_BackColor(Color.Lavender);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdClearTimesDropped);
			((Control)panel2).get_Controls().Add((Control)(object)label2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSetTimesDropped);
			((Control)panel2).get_Controls().Add((Control)(object)lstDropped);
			((Control)panel2).get_Controls().Add((Control)(object)label11);
			((Control)panel2).set_Location(new Point(316, 98));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(132, 170));
			((Control)panel2).set_TabIndex(47);
			((Control)cmdExit).set_BackColor(Color.Plum);
			((Control)cmdExit).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdExit).set_Image((Image)Resources.error);
			((ButtonBase)cmdExit).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdExit).set_Location(new Point(294, 2));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(207, 32));
			((Control)cmdExit).set_TabIndex(48);
			((Control)cmdExit).set_Text("Return to analysis (Exit)");
			((ButtonBase)cmdExit).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(false);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click_1);
			((Control)lblOCRprogress).set_AutoSize(true);
			((Control)lblOCRprogress).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)lblOCRprogress).set_Location(new Point(220, 531));
			((Control)lblOCRprogress).set_Name("lblOCRprogress");
			((Control)lblOCRprogress).set_Size(new Size(73, 13));
			((Control)lblOCRprogress).set_TabIndex(49);
			((Control)lblOCRprogress).set_Text("OCR progress");
			((Control)lblFrameRate).set_AutoSize(true);
			((Control)lblFrameRate).set_Location(new Point(563, 6));
			((Control)lblFrameRate).set_Name("lblFrameRate");
			((Control)lblFrameRate).set_Size(new Size(126, 13));
			((Control)lblFrameRate).set_TabIndex(50);
			((Control)lblFrameRate).set_Text("Frame rate variation = 0%");
			((Control)chkSecDecPlaces).set_AutoSize(true);
			chkSecDecPlaces.set_Checked(Settings.Default.AOTA_Integrity_DecPlaces);
			((Control)chkSecDecPlaces).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AOTA_Integrity_DecPlaces", true, (DataSourceUpdateMode)1));
			((Control)chkSecDecPlaces).set_Location(new Point(205, 484));
			((Control)chkSecDecPlaces).set_Name("chkSecDecPlaces");
			((Control)chkSecDecPlaces).set_Size(new Size(105, 30));
			((Control)chkSecDecPlaces).set_TabIndex(51);
			((Control)chkSecDecPlaces).set_Text("Display secs to \r\n4 decimal places");
			((ButtonBase)chkSecDecPlaces).set_UseVisualStyleBackColor(true);
			chkSecDecPlaces.add_CheckedChanged((EventHandler)chkSecDecPlaces_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1012, 553));
			((Control)this).get_Controls().Add((Control)(object)chkSecDecPlaces);
			((Control)this).get_Controls().Add((Control)(object)lblFrameRate);
			((Control)this).get_Controls().Add((Control)(object)lblOCRprogress);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)ClearAllChecks);
			((Control)this).get_Controls().Add((Control)(object)cmdInsertDroppedFrames);
			((Control)this).get_Controls().Add((Control)(object)chkFoldShort);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)panelPlot);
			((Control)this).get_Controls().Add((Control)(object)gridFrameTimes);
			((Control)this).get_Controls().Add((Control)(object)updnFirstFrame);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)cmbNumInteg);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)cmdPlotFolded);
			((Control)this).get_Controls().Add((Control)(object)cmdCheckMeasures);
			((Control)this).get_Controls().Add((Control)(object)cmdCheckDroppedDuplicates);
			((Control)this).get_Controls().Add((Control)(object)txtFrameID);
			((Control)this).get_Controls().Add((Control)(object)txtMins);
			((Control)this).get_Controls().Add((Control)(object)txtSecs);
			((Control)this).get_Controls().Add((Control)(object)txtHrs);
			((Control)this).get_Controls().Add((Control)(object)cmdSetTime);
			((Control)this).get_Controls().Add((Control)(object)label44);
			((Control)this).get_Controls().Add((Control)(object)label43);
			((Control)this).get_Controls().Add((Control)(object)label42);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).set_Enabled(false);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("IntegrityCheck");
			((Control)this).set_Text("Integrity Check");
			((Form)this).add_FormClosed(new FormClosedEventHandler(IntegrityCheck_FormClosed));
			((Form)this).add_Load((EventHandler)IntegrityCheck_Load);
			((ISupportInitialize)gridFrameTimes).EndInit();
			((ISupportInitialize)updnFirstFrame).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelPlot).ResumeLayout(false);
			((ISupportInitialize)picFold).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
