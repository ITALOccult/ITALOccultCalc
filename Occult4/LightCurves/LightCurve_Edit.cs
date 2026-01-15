using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using AOTA;
using Occult;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace LightCurves
{
	public class LightCurve_Edit : Form
	{
		private bool EditSubmitted;

		private bool StarNumbersBeingSet;

		private int RecordBeingEdited;

		private int MainLightCurvesEdited;

		private static LightCurveData LCDforEdit = new LightCurveData();

		private static List<int> OriginalLightValues = new List<int>();

		private static List<bool> OriginalLightValid = new List<bool>();

		private static decimal OriginalNumberOfPoints = default(decimal);

		private static decimal OriginalDuration = default(decimal);

		private static decimal OriginalSeconds = default(decimal);

		private static decimal OriginalMinutes = default(decimal);

		private static decimal OriginalHours = default(decimal);

		private bool UpdatingIntegration;

		private bool UpdatingTruncation;

		private bool CancelCheck;

		private IContainer components;

		private NumericUpDown updnLightCurveNumber;

		private Label lblSource;

		private Panel panel1;

		private Label label1;

		private ListBox lstErrors;

		private Button cmdCheckForErrors;

		private ProgressBar pBarErrors;

		private Label lblToEditCount;

		private Label label2;

		private NumericUpDown updnYear;

		private GroupBox groupBox1;

		private Label label12;

		private Label label13;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private NumericUpDown updnMonth;

		private NumericUpDown updnSecond;

		private NumericUpDown updnHour;

		private NumericUpDown updnMinute;

		private NumericUpDown updnDay;

		private Label lblDurn;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private GroupBox groupBox2;

		private NumericUpDown updnPoints;

		private NumericUpDown updnDuration;

		private GroupBox groupBox3;

		private NumericUpDown updnAlt;

		private Label label23;

		private Label label20;

		private Label label21;

		private Label label22;

		private NumericUpDown updnLatSec;

		private NumericUpDown updnLatDeg;

		private NumericUpDown updnLatMin;

		private Label label3;

		private Label label18;

		private Label label19;

		private NumericUpDown updnLongSec;

		private NumericUpDown updnLongDeg;

		private NumericUpDown updnLongMin;

		private TextBox txtObserver;

		private Label label25;

		private Label label24;

		private GroupBox groupBox4;

		private Label label26;

		private Label label31;

		private Label label30;

		private Label label27;

		private Label label39;

		private NumericUpDown updnU4b;

		private NumericUpDown updnU4a;

		private NumericUpDown updnSAO;

		private NumericUpDown updnXZ;

		private NumericUpDown updnZC;

		private NumericUpDown updnHip;

		private NumericUpDown updnTa;

		private NumericUpDown updnTc;

		private NumericUpDown updnTb;

		private TextBox txtNS;

		private TextBox txtEW;

		private Label lblStarLine;

		private Label lblDate;

		private Label lblLight;

		private Label lblObserver;

		private GroupBox groupAsteroid;

		private Label lblAsteroid;

		private GroupBox grpMoon;

		private NumericUpDown updnAsteroidNo;

		private TextBox txtAsteroidName;

		private Button cmdEdit;

		private Label label48;

		private Label label47;

		private Label label46;

		private Label label45;

		private Label label44;

		private Label label43;

		private Label label42;

		private Label label41;

		private Label label40;

		private Label label29;

		private Label label28;

		private NumericUpDown updnLibL;

		private NumericUpDown updnLibB;

		private NumericUpDown updnSlope;

		private NumericUpDown updnContact;

		private NumericUpDown updnRadius;

		private NumericUpDown updnMoonAlt;

		private NumericUpDown updnIllum;

		private NumericUpDown updnPA;

		private NumericUpDown updnLimbMotion;

		private NumericUpDown updnAA;

		private Label lblMoon;

		private ComboBox cmbNS;

		private NumericUpDown updnCA;

		private Button cmdGetName;

		private RadioButton optMoon;

		private RadioButton optAsteroid;

		private GroupBox groupBox5;

		private CheckBox chkExclude;

		private Button cmdUpdateSubmitted;

		private Button cmdAcceptChanges;

		private Label lblMainUpdated;

		private Label lblSubmittedAdvice;

		private Label lblMainAdvice;

		private Button cmdCheckSAO_XZ;

		private ToolTip toolTip1;

		private Button cmdDuplicates;

		private Button cmdUpdateSAO;

		private Button cmdUpdateXZ;

		private Label label34;

		private Label label33;

		private Label label32;

		private Label label35;

		private Label label36;

		private PictureBox picCheck;

		private Button cmdPlotAll;

		private Button cmdZerovalues;

		internal Panel PanelPlot;

		private Panel panel3;

		private Button cmd_x8;

		private Button cmd_x4;

		private Button cmd_x2;

		private Button cmd_x1;

		internal NumericUpDown updnScale;

		private Label label37;

		private PictureBox picLightCurve;

		private CheckBox chkTimeHeight;

		internal CheckBox chkShowSkippedFrames;

		private Panel panelSetInvalid;

		private Label label38;

		private TextBox txtEnd;

		private TextBox txtStart;

		private Button cmdSetInvalid;

		private RadioButton optEnd;

		private RadioButton optStart;

		private Button cmdResetToValid;

		private Button cmdSelectAll;

		private CheckBox chkValid;

		private Panel panel2;

		private Label label49;

		private Panel panel4;

		private Label label54;

		private Label label53;

		private Label label52;

		private Label label51;

		private Label label50;

		private Label label55;

		private Panel panel5;

		private Button cmdHelp;

		private CheckBox chkReview;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem displayEventInAOTAToolStripMenuItem;

		private ToolStripMenuItem exitSaveChangesToolStripMenuItem;

		private ToolStripMenuItem copyLightCurveToolStripMenuItem;

		private Panel pnlIntegration;

		private Label label58;

		private NumericUpDown updnFramesToCombine;

		private Label label57;

		private NumericUpDown updnFirst;

		private Label label56;

		private Button cmdIntegrate;

		private Button cmdCancel;

		private Label label59;

		private Button cmd_x16;

		private Label label60;

		private Label label61;

		private Label label62;

		private Panel panel6;

		private ToolStripMenuItem lightCurveEditingToolStripMenuItem;

		private ToolStripMenuItem integrationToolStripMenuItem;

		private ToolStripMenuItem truncationToolStripMenuItem;

		private ToolStripMenuItem binIntegrationblocksToolStripMenuItem1;

		private ToolStripMenuItem undoIntegrationToolStripMenuItem;

		private Panel pnlTruncate;

		private Label label64;

		private Label label63;

		private Button cmdTruncate;

		private Button cmdCancelTruncate;

		private Label label65;

		private NumericUpDown updnLastFrame;

		private Label label66;

		private NumericUpDown updnFirstFrame;

		private ToolStripMenuItem truncateToolStripMenuItem;

		private ToolStripMenuItem undoTruncationToolStripMenuItem;

		private Panel panel7;

		private Label label67;

		private Label label68;

		private Label label69;

		private ToolStripMenuItem invalidFramesToolStripMenuItem;

		private TextBox txtSingle;

		private RadioButton optSingle;

		private Panel panel8;

		private Button cmdSetFrameValid;

		private Button cmdSetFrameInvalid;

		private Label label70;

		private ToolStripMenuItem swapLatitudeLongitudeValuesToolStripMenuItem;

		private ToolStripMenuItem flipSignOfLongitudeToolStripMenuItem;

		private ToolStripMenuItem flipSignOfLatitudeToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem siteEditsToolStripMenuItem;

		private CheckBox chkEmbargoed;

		private Button cmdCancelCheck;

		public LightCurve_Edit(bool SubmittedFile)
		{
			InitializeComponent();
			((Control)pnlIntegration).set_Left(((Control)this).get_Width() - ((Control)pnlIntegration).get_Width() - 20);
			((Control)pnlIntegration).set_Top(((Control)PanelPlot).get_Top() - ((Control)pnlIntegration).get_Height() - 5);
			((Control)pnlTruncate).set_Left(((Control)this).get_Width() - ((Control)pnlTruncate).get_Width() - 20);
			((Control)pnlTruncate).set_Top(((Control)PanelPlot).get_Top() - ((Control)pnlTruncate).get_Height() - 5);
			((Control)panelSetInvalid).set_Left(((Control)this).get_Width() - ((Control)panelSetInvalid).get_Width() - 20);
			((Control)panelSetInvalid).set_Top(((Control)PanelPlot).get_Top() - ((Control)panelSetInvalid).get_Height() - 5);
			EditSubmitted = SubmittedFile;
			if (EditSubmitted)
			{
				((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Text("E X I T");
				toolTip1.SetToolTip((Control)(object)chkExclude, "When you click 'Update the submitted file', the\r\nextension of the original file will become '.init'\r\nand the currently displayed data entry will be\r\nsaved with the extension '.del'");
				toolTip1.SetToolTip((Control)(object)chkReview, "When you click 'Update the submitted file', the\r\nextension of the original file will become '.review'\r\nand the currently displayed data entry will be\r\nsaved with the extension '.del'");
			}
			else
			{
				((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Text("E X I T  &&  save changes");
				toolTip1.SetToolTip((Control)(object)chkExclude, "When you click 'Accept the revised data', the\r\nlight curve entry within this editor will be  flagged\r\nfor deletion. Deletion will only occur when you Exit this\r\neditor. On exit, you will be asked if you want to make the edited\r\nchanges, which includes deletion.\r\nNo copy of the excluded entry is saved.");
			}
		}

		private void LightCurve_Edit_Load(object sender, EventArgs e)
		{
			((Control)lblMainUpdated).set_Top((int)((float)((Control)cmdAcceptChanges).get_Top() + (float)(((Control)cmdAcceptChanges).get_Height() - ((Control)lblMainUpdated).get_Height()) / 2f));
			if (EditSubmitted)
			{
				((Control)this).set_Text("Edit a Submitted file");
				updnLightCurveNumber.set_Value(1m);
				updnLightCurveNumber.set_Maximum((decimal)LightData.LightCurvesSubmitted.Count);
				((Control)lblSource).set_Text("Editing Submitted light curves");
				Button obj = cmdUpdateSubmitted;
				CheckBox obj2 = chkReview;
				bool flag;
				((Control)chkEmbargoed).set_Visible(flag = true);
				bool visible;
				((Control)obj2).set_Visible(visible = flag);
				((Control)obj).set_Visible(visible);
				Button obj3 = cmdAcceptChanges;
				((Control)lblMainUpdated).set_Visible(visible = false);
				((Control)obj3).set_Visible(visible);
				((Control)lblSubmittedAdvice).set_Visible(true);
				((Control)lblMainAdvice).set_Visible(false);
			}
			else
			{
				((Control)this).set_Text("Edit the Main file");
				updnLightCurveNumber.set_Value(1m);
				updnLightCurveNumber.set_Maximum((decimal)LightData.MainLightCurves.Count);
				((Control)lblSource).set_Text("Editing Main light curves");
				Button obj4 = cmdUpdateSubmitted;
				CheckBox obj5 = chkReview;
				bool flag;
				((Control)chkEmbargoed).set_Visible(flag = false);
				bool visible;
				((Control)obj5).set_Visible(visible = flag);
				((Control)obj4).set_Visible(visible);
				Button obj6 = cmdAcceptChanges;
				((Control)lblMainUpdated).set_Visible(visible = true);
				((Control)obj6).set_Visible(visible);
				MainLightCurvesEdited = 0;
				((Control)lblSubmittedAdvice).set_Visible(false);
				((Control)lblMainAdvice).set_Visible(true);
			}
			chkEmbargoed.set_Checked(true);
		}

		private void cmdCheckForErrors_Click(object sender, EventArgs e)
		{
			((Control)lstErrors).set_BackColor(Color.Honeydew);
			lstErrors.get_Items().Clear();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			((Control)pBarErrors).set_Visible(true);
			((Control)lblToEditCount).set_Visible(false);
			if (EditSubmitted)
			{
				pBarErrors.set_Maximum(LightData.LightCurvesSubmitted.Count);
				for (int i = 0; i < LightData.LightCurvesSubmitted.Count; i++)
				{
					pBarErrors.set_Value(i);
					Application.DoEvents();
					if ((!chkEmbargoed.get_Checked() || !LightData.LightCurvesSubmitted[i].FileName.Contains("embargoed")) && LightData.LightCurvesSubmitted[i].ErrorCheck())
					{
						lstErrors.get_Items().Add((object)(i + 1));
					}
				}
			}
			else
			{
				pBarErrors.set_Maximum(LightData.MainLightCurves.Count);
				((Control)cmdCancelCheck).set_Visible(true);
				CancelCheck = false;
				for (int j = 0; j < LightData.MainLightCurves.Count; j++)
				{
					pBarErrors.set_Value(j);
					Application.DoEvents();
					if (CancelCheck)
					{
						break;
					}
					if (LightData.MainLightCurves[j].ErrorCheck())
					{
						lstErrors.get_Items().Add((object)(j + 1));
					}
				}
				CancelCheck = false;
				((Control)cmdCancelCheck).set_Visible(false);
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			((Control)pBarErrors).set_Visible(false);
			((Control)lblToEditCount).set_Text(lstErrors.get_Items().get_Count() + " curves to edit");
			((Control)lblToEditCount).set_Visible(true);
		}

		private void lstErrors_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			RecordBeingEdited = int.Parse(lstErrors.get_Items().get_Item(((ListControl)lstErrors).get_SelectedIndex()).ToString()) - 1;
			updnScale.set_Value(1m);
			((ScrollProperties)((ScrollableControl)PanelPlot).get_HorizontalScroll()).set_Value(0);
			if (EditSubmitted)
			{
				LCDforEdit = LightData.LightCurvesSubmitted[RecordBeingEdited];
			}
			else
			{
				LCDforEdit = LightData.MainLightCurves[RecordBeingEdited];
			}
			DecodeRecordBeingEdited();
			TextBox obj = txtStart;
			string text;
			((Control)txtEnd).set_Text(text = "");
			((Control)obj).set_Text(text);
			((Control)picCheck).set_Visible(false);
		}

		private void cmdEdit_Click(object sender, EventArgs e)
		{
			Edit();
		}

		private void Edit()
		{
			RadioButton obj = optSingle;
			RadioButton obj2 = optStart;
			bool flag;
			optEnd.set_Checked(flag = false);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			TextBox obj3 = txtStart;
			string text;
			((Control)txtEnd).set_Text(text = "");
			((Control)obj3).set_Text(text);
			RecordBeingEdited = (int)updnLightCurveNumber.get_Value() - 1;
			updnScale.set_Value(1m);
			((ScrollProperties)((ScrollableControl)PanelPlot).get_HorizontalScroll()).set_Value(0);
			LCDforEdit = new LightCurveData();
			if (EditSubmitted)
			{
				LCDforEdit = LightData.LightCurvesSubmitted[RecordBeingEdited];
			}
			else
			{
				LCDforEdit = LightData.MainLightCurves[RecordBeingEdited];
			}
			DecodeRecordBeingEdited();
			((Control)picCheck).set_Visible(false);
			ResetIntegrationMenuEnabling();
			ResetTruncationMenuEnabling();
		}

		private void DecodeRecordBeingEdited()
		{
			//IL_12f2: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = "  ||  ";
			updnYear.set_Value(0m);
			updnMonth.set_Value(0m);
			updnDay.set_Value(0m);
			updnHour.set_Value(0m);
			updnMinute.set_Value(0m);
			updnSecond.set_Value(0m);
			updnDuration.set_Value(0m);
			updnPoints.set_Value(0m);
			((Control)txtObserver).set_Text("");
			((Control)txtEW).set_Text("");
			updnLongDeg.set_Value(0m);
			updnLongMin.set_Value(0m);
			updnLongSec.set_Value(0m);
			updnLatDeg.set_Value(0m);
			((Control)txtNS).set_Text("");
			updnLatMin.set_Value(0m);
			updnLatSec.set_Value(0m);
			updnAlt.set_Value(0m);
			updnHip.set_Value(0m);
			updnTa.set_Value(0m);
			updnTb.set_Value(0m);
			updnTc.set_Value(0m);
			updnU4a.set_Value(0m);
			updnU4b.set_Value(0m);
			updnSAO.set_Value(0m);
			updnXZ.set_Value(0m);
			updnZC.set_Value(0m);
			updnAsteroidNo.set_Value(0m);
			((Control)txtAsteroidName).set_Text("");
			updnAA.set_Value(0m);
			updnLibL.set_Value(0m);
			updnLibB.set_Value(0m);
			updnSlope.set_Value(0m);
			updnLimbMotion.set_Value(0m);
			updnContact.set_Value(0m);
			updnRadius.set_Value(1m);
			updnPA.set_Value(0m);
			updnCA.set_Value(0m);
			((ListControl)cmbNS).set_SelectedIndex(0);
			updnIllum.set_Value(0m);
			updnAlt.set_Value(0m);
			chkExclude.set_Checked(LCDforEdit.ExcludeLightCurve);
			chkReview.set_Checked(LCDforEdit.ReviewLightCurve);
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Date:");
			stringBuilder.AppendFormat(" {0:f0}-", LCDforEdit.Year);
			stringBuilder.AppendFormat("{0:f0}-", LCDforEdit.Month);
			stringBuilder.AppendFormat("{0:f0} " + text2, LCDforEdit.Day);
			stringBuilder.AppendFormat("{0:f0}" + text2, LCDforEdit.Hr);
			stringBuilder.AppendFormat("{0:f0}" + text2, LCDforEdit.Min);
			stringBuilder.AppendFormat("{0:f2}" + text2, LCDforEdit.Sec);
			((Control)lblDate).set_Text(stringBuilder.ToString());
			try
			{
				updnYear.set_Value((decimal)LCDforEdit.Year);
			}
			catch
			{
			}
			try
			{
				updnMonth.set_Value((decimal)LCDforEdit.Month);
			}
			catch
			{
			}
			try
			{
				updnDay.set_Value((decimal)LCDforEdit.Day);
			}
			catch
			{
			}
			try
			{
				updnHour.set_Value((decimal)LCDforEdit.Hr);
			}
			catch
			{
			}
			try
			{
				updnMinute.set_Value((decimal)LCDforEdit.Min);
			}
			catch
			{
			}
			try
			{
				updnSecond.set_Value((decimal)LCDforEdit.Sec);
			}
			catch
			{
			}
			stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0:f2}" + text2, LCDforEdit.Duration);
			stringBuilder.AppendFormat(" {0:f0}", LCDforEdit.NumPoints);
			((Control)lblLight).set_Text(stringBuilder.ToString());
			try
			{
				updnDuration.set_Value((decimal)LCDforEdit.Duration);
			}
			catch
			{
			}
			try
			{
				updnPoints.set_Value((decimal)LCDforEdit.NumPoints);
			}
			catch
			{
			}
			stringBuilder = new StringBuilder();
			stringBuilder.Append("Observer" + text2);
			stringBuilder.Append(" " + LCDforEdit.Observer + text2);
			stringBuilder.AppendFormat(" {0}{1:f0}:{2:f0}:{3:f1}" + text2, LCDforEdit.EW, LCDforEdit.LongD, LCDforEdit.LongM, LCDforEdit.LongS);
			stringBuilder.AppendFormat(" {0}{1:f0}:{2:f0}:{3:f1}" + text2, LCDforEdit.NS, LCDforEdit.LatD, LCDforEdit.LatM, LCDforEdit.LatS);
			stringBuilder.AppendFormat(" {0:f0}" + text2, LCDforEdit.AltM);
			((Control)lblObserver).set_Text(stringBuilder.ToString());
			try
			{
				((Control)txtObserver).set_Text(LCDforEdit.Observer);
			}
			catch
			{
			}
			try
			{
				((Control)txtEW).set_Text(LCDforEdit.EW);
			}
			catch
			{
			}
			try
			{
				updnLongDeg.set_Value((decimal)LCDforEdit.LongD);
			}
			catch
			{
			}
			try
			{
				updnLongMin.set_Value((decimal)LCDforEdit.LongM);
			}
			catch
			{
			}
			try
			{
				updnLongSec.set_Value((decimal)LCDforEdit.LongS);
			}
			catch
			{
			}
			try
			{
				updnLatDeg.set_Value((decimal)LCDforEdit.LatD);
			}
			catch
			{
			}
			try
			{
				((Control)txtNS).set_Text(LCDforEdit.NS);
			}
			catch
			{
			}
			try
			{
				updnLatMin.set_Value((decimal)LCDforEdit.LatM);
			}
			catch
			{
			}
			try
			{
				updnLatSec.set_Value((decimal)LCDforEdit.LatS);
			}
			catch
			{
			}
			try
			{
				updnAlt.set_Value((decimal)LCDforEdit.AltM);
			}
			catch
			{
			}
			stringBuilder = new StringBuilder();
			stringBuilder.Append("Star" + text2);
			stringBuilder.AppendFormat(" Hip {0:f0}" + text2, LCDforEdit.Hipparcos);
			stringBuilder.AppendFormat(" Tycho2 {0:f0}-{1:f0}-{2:f0}" + text2, LCDforEdit.Tyc2A, LCDforEdit.Tyc2B, LCDforEdit.Tyc2C);
			stringBuilder.AppendFormat(" UCAC4 {0:f0}-{1:f0}" + text2, LCDforEdit.U4Zone, LCDforEdit.U4Number);
			stringBuilder.AppendFormat(" SAO {0:f0}" + text2, LCDforEdit.SAO);
			stringBuilder.AppendFormat(" XZ {0:f0}" + text2, LCDforEdit.XZ);
			stringBuilder.AppendFormat(" ZC {0:f0}" + text2, LCDforEdit.ZC);
			((Control)lblStarLine).set_Text(stringBuilder.ToString());
			StarNumbersBeingSet = true;
			try
			{
				updnHip.set_Value((decimal)LCDforEdit.Hipparcos);
			}
			catch
			{
			}
			try
			{
				updnTa.set_Value((decimal)LCDforEdit.Tyc2A);
			}
			catch
			{
			}
			try
			{
				updnTb.set_Value((decimal)LCDforEdit.Tyc2B);
			}
			catch
			{
			}
			try
			{
				updnTc.set_Value((decimal)LCDforEdit.Tyc2C);
			}
			catch
			{
			}
			try
			{
				updnU4a.set_Value((decimal)LCDforEdit.U4Zone);
			}
			catch
			{
			}
			try
			{
				updnU4b.set_Value((decimal)LCDforEdit.U4Number);
			}
			catch
			{
			}
			try
			{
				updnSAO.set_Value((decimal)LCDforEdit.SAO);
			}
			catch
			{
			}
			try
			{
				updnXZ.set_Value((decimal)LCDforEdit.XZ);
			}
			catch
			{
			}
			try
			{
				updnZC.set_Value((decimal)LCDforEdit.ZC);
			}
			catch
			{
			}
			StarNumbersBeingSet = false;
			RadioButton obj28 = optAsteroid;
			bool UsedGaia;
			((Control)groupAsteroid).set_Enabled(UsedGaia = LCDforEdit.MoonSize > 2.0);
			obj28.set_Checked(UsedGaia);
			optAsteroid.set_Checked(LCDforEdit.MoonSize > 2.0);
			optMoon.set_Checked(!optAsteroid.get_Checked());
			SetAsteroid_Moon();
			((Control)grpMoon).set_Enabled(!((Control)groupAsteroid).get_Enabled());
			stringBuilder = new StringBuilder();
			stringBuilder.Append(" Asteroid" + text2);
			stringBuilder.AppendFormat(" {0}" + text2, LCDforEdit.AsteroidNumber);
			stringBuilder.Append(" " + LCDforEdit.AsteroidName);
			((Control)lblAsteroid).set_Text(stringBuilder.ToString());
			try
			{
				updnAsteroidNo.set_Value((decimal)LCDforEdit.AsteroidNumber);
			}
			catch
			{
			}
			try
			{
				((Control)txtAsteroidName).set_Text(LCDforEdit.AsteroidName);
			}
			catch
			{
			}
			if (LCDforEdit.NumPoints > 3)
			{
				PlotEditorLightCurve();
			}
			stringBuilder = new StringBuilder();
			stringBuilder.Append(" Moon" + text2);
			stringBuilder.AppendFormat(" {0:f3}" + text2, LCDforEdit.AxisAngle);
			stringBuilder.AppendFormat(" {0:f3}" + text2, LCDforEdit.Lib_L);
			stringBuilder.AppendFormat(" {0:f3}" + text2, LCDforEdit.Lib_B);
			stringBuilder.AppendFormat(" {0:f2}" + text2, LCDforEdit.LimbSlope);
			stringBuilder.AppendFormat(" {0:f4}" + text2, LCDforEdit.NormalMotion);
			stringBuilder.AppendFormat(" {0:f2}" + text2, LCDforEdit.ContactAngle);
			stringBuilder.AppendFormat(" {0:f3}" + text2, LCDforEdit.MoonSize);
			stringBuilder.AppendFormat(" {0:f2}" + text2, LCDforEdit.PosAngle);
			stringBuilder.AppendFormat(" {0}" + text2, LCDforEdit.CuspAngle);
			stringBuilder.AppendFormat(" {0}" + text2, LCDforEdit.Illumination);
			stringBuilder.AppendFormat(" {0}", LCDforEdit.MoonALt);
			((Control)lblMoon).set_Text(stringBuilder.ToString());
			try
			{
				updnAA.set_Value((decimal)LCDforEdit.AxisAngle);
			}
			catch
			{
			}
			try
			{
				updnLibL.set_Value((decimal)LCDforEdit.Lib_L);
			}
			catch
			{
			}
			try
			{
				updnLibB.set_Value((decimal)LCDforEdit.Lib_B);
			}
			catch
			{
			}
			try
			{
				updnSlope.set_Value((decimal)LCDforEdit.LimbSlope);
			}
			catch
			{
			}
			try
			{
				updnLimbMotion.set_Value((decimal)LCDforEdit.NormalMotion);
			}
			catch
			{
			}
			try
			{
				updnContact.set_Value((decimal)LCDforEdit.ContactAngle);
			}
			catch
			{
			}
			try
			{
				updnRadius.set_Value((decimal)LCDforEdit.MoonSize);
			}
			catch
			{
			}
			try
			{
				updnPA.set_Value((decimal)LCDforEdit.PosAngle);
			}
			catch
			{
			}
			string text3 = LCDforEdit.CuspAngle.Trim();
			try
			{
				updnCA.set_Value((decimal)int.Parse(text3.Substring(0, text3.Length - 1)));
			}
			catch
			{
			}
			try
			{
				((ListControl)cmbNS).set_SelectedIndex("NESW".IndexOf(text3.Substring(text3.Length - 1)));
			}
			catch
			{
			}
			try
			{
				updnIllum.set_Value((decimal)LCDforEdit.Illumination);
			}
			catch
			{
			}
			try
			{
				updnMoonAlt.set_Value((decimal)LCDforEdit.MoonALt);
			}
			catch
			{
			}
			if ((updnYear.get_Value() == 0m) | (updnMonth.get_Value() == 0m) | (updnDay.get_Value() == 0m) | ((updnHour.get_Value() == 0m) & (updnMinute.get_Value() == 0m) & (updnSecond.get_Value() == 0m)))
			{
				text += "Time, ";
			}
			if ((((Control)txtObserver).get_Text().Trim() == "") | ((updnLongDeg.get_Value() == 0m) & (updnLongMin.get_Value() == 0m) & (updnLongSec.get_Value() == 0m)) | ((updnLatDeg.get_Value() == 0m) & (updnLatMin.get_Value() == 0m) & (updnLatSec.get_Value() == 0m)))
			{
				text += "Observer, ";
			}
			if ((updnDuration.get_Value() < 0.5m) | (updnPoints.get_Value() < 10m))
			{
				text += "T length, ";
			}
			double RA = 0.0;
			double Dec = 0.0;
			bool flag = false;
			double Epoch;
			double Parallax;
			double MagR;
			double MagB;
			double MagV;
			double pmDec;
			double pmRA;
			if (!flag & (updnU4b.get_Value() > 0m))
			{
				flag = GetStarPosition.GetUCAC4Position((int)updnU4a.get_Value(), (int)updnU4b.get_Value(), Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out Epoch, out Parallax, out MagR, out MagB, out MagV, out pmDec, out UsedGaia, out pmRA);
			}
			if (!flag & (updnTb.get_Value() > 0m))
			{
				flag = GetStarPosition.GetTycho2Position((int)updnTa.get_Value(), (int)updnTb.get_Value(), (int)updnTc.get_Value(), out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch);
			}
			if (!flag & (updnHip.get_Value() > 0m))
			{
				flag = GetStarPosition.GetHipparcosPosition((int)updnHip.get_Value(), out RA, out Dec, out Epoch, out Parallax, out MagR, out MagB, out MagV, out pmDec, out pmRA, out UsedGaia, out var _);
			}
			if (!flag & (updnXZ.get_Value() > 0m))
			{
				flag = GetStarPosition.GetXZPosition((int)updnXZ.get_Value(), out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax);
			}
			if (!flag)
			{
				text += "Star number, ";
			}
			if (LCDforEdit.MoonSize > 2.0)
			{
				if ((updnAsteroidNo.get_Value() == 0m) | (((Control)txtAsteroidName).get_Text().Trim() == ""))
				{
					text += "asteroid,";
				}
				if (updnAsteroidNo.get_Value() > 0m && LightCurveData.Asteroids.GetAsteroidName_fromNumber((int)updnAsteroidNo.get_Value(), out Parallax) != ((Control)txtAsteroidName).get_Text().Trim())
				{
					text += "asteroid name,";
				}
			}
			else
			{
				int num = (int)updnXZ.get_Value();
				int num2 = (int)updnSAO.get_Value();
				if (num > 0)
				{
					XZ80Q.Get_XZ_Star(num);
					if (XZ80Q.SAO != num2)
					{
						text += "SAO/XZ, ";
					}
				}
				else if (num2 > 0)
				{
					XZ80Q.Get_SAO_Star(num2);
					if (XZ80Q.SAO != num2)
					{
						text += "SAO/XZ, ";
					}
				}
				else
				{
					text += "SAO/XZ, ";
				}
				if ((updnAA.get_Value() == 0m) | (updnLibL.get_Value() == 0m) | (updnLibB.get_Value() == 0m) | (updnSlope.get_Value() == 0m) | (updnContact.get_Value() == 0m) | (updnRadius.get_Value() == 0m) | (updnPA.get_Value() == 0m) | (updnIllum.get_Value() == 0m) | (updnMoonAlt.get_Value() == 0m))
				{
					text += "moon";
				}
			}
			if (text.Length > 0)
			{
				MessageBox.Show(text, "Errors", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void PlotEditorLightCurve()
		{
			if (!UpdatingIntegration && !UpdatingTruncation && LCDforEdit.NumPoints != 0)
			{
				((Control)picLightCurve).set_Width((int)((float)((Control)PanelPlot).get_Width() * (float)updnScale.get_Value()) - 6);
				((Control)picLightCurve).set_Height(((Control)PanelPlot).get_Height() - 17);
				float plotScale = (float)((double)(((Control)picLightCurve).get_Width() - 1) / (double)LCDforEdit.NumPoints);
				int result = -1;
				int result2 = -1;
				int firstIntegration_TruncationFrame = -1;
				int framesToCombine = -1;
				int lastTruncationFrame = -1;
				if (((Control)panelSetInvalid).get_Visible())
				{
					int.TryParse(((Control)txtStart).get_Text(), out result);
					int.TryParse(((Control)txtEnd).get_Text(), out result2);
				}
				if (((Control)pnlIntegration).get_Visible())
				{
					firstIntegration_TruncationFrame = (int)updnFirst.get_Value();
					framesToCombine = (int)updnFramesToCombine.get_Value();
				}
				if (((Control)pnlTruncate).get_Visible())
				{
					firstIntegration_TruncationFrame = (int)updnFirstFrame.get_Value();
					lastTruncationFrame = (int)updnLastFrame.get_Value();
				}
				((Control)this).Focus();
				string Header;
				if (EditSubmitted)
				{
					picLightCurve.set_Image((Image)LightData.LightCurveImage(RecordBeingEdited, ((Control)picLightCurve).get_Width(), ((Control)picLightCurve).get_Height() - 4, 0, plotScale, 1f, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, chkShowSkippedFrames.get_Checked() & ((Control)PanelPlot).get_Visible(), result, result2, firstIntegration_TruncationFrame, framesToCombine, ((Control)pnlTruncate).get_Visible(), lastTruncationFrame, IsMultiplot: false, ThickLines: false, out Header));
				}
				else
				{
					picLightCurve.set_Image((Image)LightData.LightCurveImage(RecordBeingEdited, ((Control)picLightCurve).get_Width(), ((Control)picLightCurve).get_Height() - 4, 0, plotScale, 1f, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, chkShowSkippedFrames.get_Checked() & ((Control)PanelPlot).get_Visible(), result, result2, firstIntegration_TruncationFrame, framesToCombine, ((Control)pnlTruncate).get_Visible(), lastTruncationFrame, IsMultiplot: false, ThickLines: false, out Header));
				}
			}
		}

		private void EncodeRecordBeingEdited()
		{
			LCDforEdit.ExcludeLightCurve = chkExclude.get_Checked();
			LCDforEdit.ReviewLightCurve = chkReview.get_Checked();
			LCDforEdit.Year = (int)updnYear.get_Value();
			LCDforEdit.Month = (int)updnMonth.get_Value();
			LCDforEdit.Day = (int)updnDay.get_Value();
			LCDforEdit.Hr = (int)updnHour.get_Value();
			LCDforEdit.Min = (int)updnMinute.get_Value();
			LCDforEdit.Sec = (double)updnSecond.get_Value();
			LCDforEdit.Duration = (double)updnDuration.get_Value();
			LCDforEdit.NumPoints = (int)updnPoints.get_Value();
			LCDforEdit.Observer = ((Control)txtObserver).get_Text().Trim();
			LCDforEdit.EW = ((Control)txtEW).get_Text().Trim();
			LCDforEdit.LongD = (int)updnLongDeg.get_Value();
			LCDforEdit.LongM = (int)updnLongMin.get_Value();
			LCDforEdit.LongS = (double)updnLongSec.get_Value();
			LCDforEdit.LatD = (int)updnLatDeg.get_Value();
			LCDforEdit.NS = ((Control)txtNS).get_Text().Trim();
			LCDforEdit.LatM = (int)updnLatMin.get_Value();
			LCDforEdit.LatS = (double)updnLatSec.get_Value();
			LCDforEdit.AltM = (int)updnAlt.get_Value();
			LCDforEdit.Hipparcos = (int)updnHip.get_Value();
			LCDforEdit.Tyc2A = (short)updnTa.get_Value();
			LCDforEdit.Tyc2B = (short)updnTb.get_Value();
			LCDforEdit.Tyc2C = (short)updnTc.get_Value();
			LCDforEdit.U4Zone = (int)updnU4a.get_Value();
			LCDforEdit.U4Number = (int)updnU4b.get_Value();
			LCDforEdit.SAO = (int)updnSAO.get_Value();
			LCDforEdit.XZ = (int)updnXZ.get_Value();
			LCDforEdit.AsteroidNumber = (int)updnAsteroidNo.get_Value();
			LCDforEdit.AsteroidName = ((Control)txtAsteroidName).get_Text().Trim();
			LCDforEdit.AxisAngle = (double)updnAA.get_Value();
			LCDforEdit.Lib_L = (double)updnLibL.get_Value();
			LCDforEdit.Lib_B = (double)updnLibB.get_Value();
			LCDforEdit.LimbSlope = (double)updnSlope.get_Value();
			LCDforEdit.NormalMotion = (double)updnLimbMotion.get_Value();
			LCDforEdit.ContactAngle = (double)updnContact.get_Value();
			if (optMoon.get_Checked())
			{
				LCDforEdit.MoonSize = (double)updnRadius.get_Value();
			}
			else
			{
				LCDforEdit.MoonSize = 100.0;
			}
			LCDforEdit.PosAngle = (double)updnPA.get_Value();
			LCDforEdit.CuspAngle = string.Format("{0,1:f0}", updnCA.get_Value()) + "NESW".Substring(((ListControl)cmbNS).get_SelectedIndex(), 1);
			LCDforEdit.Illumination = (int)updnIllum.get_Value();
			LCDforEdit.MoonALt = (int)updnMoonAlt.get_Value();
		}

		private void LightCurve_Edit_FormClosing(object sender, FormClosingEventArgs e)
		{
			Panel pnlSelectStar = LightData.LightCurveView.pnlSelectStar;
			bool enabled;
			((ToolStripItem)LightData.LightCurveView.adminFunctionsToolStripMenuItem).set_Enabled(enabled = true);
			((Control)pnlSelectStar).set_Enabled(enabled);
		}

		private void cmdGetName_Click(object sender, EventArgs e)
		{
			int asteroidNumber = (int)updnAsteroidNo.get_Value();
			((Control)txtAsteroidName).set_Text(LightCurveData.Asteroids.GetAsteroidName_fromNumber(asteroidNumber, out var _));
		}

		private void optAsteroid_CheckedChanged(object sender, EventArgs e)
		{
			SetAsteroid_Moon();
		}

		private void SetAsteroid_Moon()
		{
			if (optAsteroid.get_Checked())
			{
				((Control)groupAsteroid).set_Enabled(true);
				((Control)grpMoon).set_Enabled(false);
			}
			else
			{
				((Control)groupAsteroid).set_Enabled(false);
				((Control)grpMoon).set_Enabled(true);
			}
		}

		private void updnSAO_ValueChanged(object sender, EventArgs e)
		{
			if (!StarNumbersBeingSet)
			{
				if (XZ80Q.Get_SAO_Star((int)updnSAO.get_Value()))
				{
					StarNumbersBeingSet = true;
					updnZC.set_Value((decimal)XZ80Q.ZC);
					updnXZ.set_Value((decimal)XZ80Q.XZ);
					StarNumbersBeingSet = false;
				}
				else
				{
					NumericUpDown obj = updnZC;
					NumericUpDown obj2 = updnXZ;
					decimal value = default(decimal);
					obj2.set_Value(value);
					obj.set_Value(value);
				}
			}
		}

		private void updnZC_ValueChanged(object sender, EventArgs e)
		{
			if (!StarNumbersBeingSet)
			{
				if (XZ80Q.Get_ZC_Star((int)updnZC.get_Value()))
				{
					StarNumbersBeingSet = true;
					updnSAO.set_Value((decimal)XZ80Q.SAO);
					updnXZ.set_Value((decimal)XZ80Q.XZ);
					StarNumbersBeingSet = false;
				}
				else
				{
					NumericUpDown obj = updnSAO;
					NumericUpDown obj2 = updnXZ;
					decimal value = default(decimal);
					obj2.set_Value(value);
					obj.set_Value(value);
				}
			}
		}

		private void cmdUpdateSubmitted_Click(object sender, EventArgs e)
		{
			EncodeRecordBeingEdited();
			string text = Utilities.AppPath + "\\LightCurveReports\\";
			string text2 = Path.GetFileName(LCDforEdit.FileName);
			string text3 = text2.Replace(".dat", ".init");
			if (!File.Exists(text + text3))
			{
				File.Move(text + text2, text + text3);
			}
			if (File.Exists(text + text2))
			{
				File.Delete(text + text2);
			}
			string text4 = "";
			if (chkExclude.get_Checked())
			{
				text4 = ".del";
			}
			else if (chkReview.get_Checked())
			{
				text4 = ".review";
			}
			else
			{
				text2 = text2.Replace(".review", "");
				text4 = "";
			}
			using (StreamWriter streamWriter = new StreamWriter(text + text2 + text4))
			{
				streamWriter.Write(LCDforEdit.ToString());
			}
			((Control)picCheck).set_Visible(true);
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Enabled(false);
		}

		private void cmdUpdateInMemory_Click(object sender, EventArgs e)
		{
			EncodeRecordBeingEdited();
			MainLightCurvesEdited++;
			((Control)picCheck).set_Visible(true);
			((Control)lblMainUpdated).set_Text("Light curves updated:  " + MainLightCurvesEdited);
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Enabled(false);
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008b: Invalid comparison between Unknown and I4
			if (EditSubmitted)
			{
				((Form)this).Close();
				return;
			}
			if (MainLightCurvesEdited > 0 && (int)MessageBox.Show("Do you want to save the edited main observations", "Save edited", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				int num = 0;
				bool removeExcluded = false;
				for (int i = 0; i < LightData.MainLightCurves.Count; i++)
				{
					if (LightData.MainLightCurves[i].ExcludeLightCurve)
					{
						num++;
					}
				}
				if (num > 0 && (int)MessageBox.Show(num + " light curves are flagged to be excluded\r\n\r\nIf they are to be excluded, this must happen with this Save\r\nOtherwise the flag will be removed\r\n\r\nDo you want to excluded flagged events?", "Exclude flagged events", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					removeExcluded = true;
				}
				LightData.SaveMainLightCurves(removeExcluded);
			}
			((Form)this).Close();
		}

		private void cmdCheckSAO_XZ_Click(object sender, EventArgs e)
		{
			((Control)lstErrors).set_BackColor(Color.LightCyan);
			lstErrors.get_Items().Clear();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			((Control)pBarErrors).set_Visible(true);
			((Control)lblToEditCount).set_Visible(false);
			if (EditSubmitted)
			{
				pBarErrors.set_Maximum(LightData.LightCurvesSubmitted.Count);
				for (int i = 0; i < LightData.LightCurvesSubmitted.Count; i++)
				{
					pBarErrors.set_Value(i);
					Application.DoEvents();
					if (LightData.LightCurvesSubmitted[i].MoonSize > 2.0)
					{
						continue;
					}
					int xZ = LightData.LightCurvesSubmitted[i].XZ;
					int sAO = LightData.LightCurvesSubmitted[i].SAO;
					if (xZ > 0)
					{
						XZ80Q.Get_XZ_Star(xZ);
						if (XZ80Q.SAO != LightData.LightCurvesSubmitted[i].SAO)
						{
							lstErrors.get_Items().Add((object)(i + 1));
						}
					}
					else if (sAO > 0)
					{
						XZ80Q.Get_SAO_Star(sAO);
						if (XZ80Q.XZ != LightData.LightCurvesSubmitted[i].XZ)
						{
							lstErrors.get_Items().Add((object)(i + 1));
						}
					}
				}
			}
			else
			{
				pBarErrors.set_Maximum(LightData.MainLightCurves.Count);
				for (int j = 0; j < LightData.MainLightCurves.Count; j++)
				{
					pBarErrors.set_Value(j);
					Application.DoEvents();
					if (LightData.MainLightCurves[j].MoonSize > 2.0)
					{
						continue;
					}
					int xZ = LightData.MainLightCurves[j].XZ;
					int sAO = LightData.MainLightCurves[j].SAO;
					if (xZ > 0)
					{
						XZ80Q.Get_XZ_Star(xZ);
						if (XZ80Q.SAO != LightData.MainLightCurves[j].SAO)
						{
							lstErrors.get_Items().Add((object)(j + 1));
						}
					}
					else if (sAO > 0)
					{
						XZ80Q.Get_SAO_Star(sAO);
						if (XZ80Q.XZ != LightData.MainLightCurves[j].XZ)
						{
							lstErrors.get_Items().Add((object)(j + 1));
						}
					}
					else
					{
						lstErrors.get_Items().Add((object)(j + 1));
					}
				}
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			((Control)pBarErrors).set_Visible(false);
			((Control)lblToEditCount).set_Text(lstErrors.get_Items().get_Count() + " curves to edit");
			((Control)lblToEditCount).set_Visible(true);
		}

		private void cmdUpdateXZ_Click(object sender, EventArgs e)
		{
			//IL_005b: Unknown result type (might be due to invalid IL or missing references)
			int sAO = (int)updnSAO.get_Value();
			if (XZ80Q.Get_SAO_Star(sAO))
			{
				StarNumbersBeingSet = true;
				updnXZ.set_Value((decimal)XZ80Q.XZ);
				StarNumbersBeingSet = false;
			}
			else
			{
				MessageBox.Show("SAO " + sAO + " is not in the XZ catalogue", "Invalid SAO", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void cmdUpdateSAO_Click(object sender, EventArgs e)
		{
			//IL_005b: Unknown result type (might be due to invalid IL or missing references)
			int xZ = (int)updnXZ.get_Value();
			if (XZ80Q.Get_XZ_Star(xZ))
			{
				StarNumbersBeingSet = true;
				updnSAO.set_Value((decimal)XZ80Q.SAO);
				StarNumbersBeingSet = false;
			}
			else
			{
				MessageBox.Show("XZ " + xZ + " is not in the XZ catalogue", "Invalid XZ", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void cmdDuplicates_Click(object sender, EventArgs e)
		{
			((Control)lstErrors).set_BackColor(Color.LightYellow);
			lstErrors.get_Items().Clear();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			((Control)pBarErrors).set_Visible(true);
			((Control)lblToEditCount).set_Visible(false);
			if (EditSubmitted)
			{
				pBarErrors.set_Maximum(LightData.LightCurvesSubmitted.Count);
				for (int i = 0; i < LightData.LightCurvesSubmitted.Count - 1; i++)
				{
					pBarErrors.set_Value(i);
					Application.DoEvents();
					if (LightData.LightCurvesSubmitted[i].Year != LightData.LightCurvesSubmitted[i + 1].Year || LightData.LightCurvesSubmitted[i].Month != LightData.LightCurvesSubmitted[i + 1].Month || LightData.LightCurvesSubmitted[i].Day != LightData.LightCurvesSubmitted[i + 1].Day)
					{
						continue;
					}
					double num = (double)LightData.LightCurvesSubmitted[i].LongD + (double)LightData.LightCurvesSubmitted[i].LongM / 60.0 + LightData.LightCurvesSubmitted[i].LongS / 3600.0;
					double num2 = (double)LightData.LightCurvesSubmitted[i + 1].LongD + (double)LightData.LightCurvesSubmitted[i + 1].LongM / 60.0 + LightData.LightCurvesSubmitted[i + 1].LongS / 3600.0;
					if (Math.Abs(num - num2) > 0.001)
					{
						continue;
					}
					double num3 = (double)LightData.LightCurvesSubmitted[i].LatD + (double)LightData.LightCurvesSubmitted[i].LatM / 60.0 + LightData.LightCurvesSubmitted[i].LatS / 3600.0;
					double num4 = (double)LightData.LightCurvesSubmitted[i + 1].LatD + (double)LightData.LightCurvesSubmitted[i + 1].LatM / 60.0 + LightData.LightCurvesSubmitted[i + 1].LatS / 3600.0;
					if (Math.Abs(num3 - num4) > 0.001)
					{
						continue;
					}
					string text = LightData.LightCurvesSubmitted[i].Observer.Trim().ToLower();
					int num5 = text.LastIndexOf(" ");
					if (num5 > 0)
					{
						text = text.Substring(num5);
					}
					string text2 = LightData.LightCurvesSubmitted[i + 1].Observer.Trim().ToLower();
					num5 = text2.LastIndexOf(" ");
					if (num5 > 0)
					{
						text2 = text2.Substring(num5);
					}
					if (text2 != text || LightData.LightCurvesSubmitted[i].MoonSize > 2.0 != LightData.LightCurvesSubmitted[i + 1].MoonSize > 2.0)
					{
						continue;
					}
					double num6 = (double)LightData.LightCurvesSubmitted[i].Hr + (double)LightData.LightCurvesSubmitted[i].Min / 60.0 + LightData.LightCurvesSubmitted[i].Sec / 3600.0;
					double num7 = (double)LightData.LightCurvesSubmitted[i + 1].Hr + (double)LightData.LightCurvesSubmitted[i + 1].Min / 60.0 + LightData.LightCurvesSubmitted[i + 1].Sec / 3600.0;
					if (LightData.LightCurvesSubmitted[i].MoonSize > 2.0)
					{
						if (LightData.LightCurvesSubmitted[i].AsteroidNumber != LightData.LightCurvesSubmitted[i + 1].AsteroidNumber || ((Math.Abs(num7 - num6) > 0.018) & (Math.Abs(num7 - num6) < 0.9982)) || Math.Abs(num7 - num6) > 1.0018)
						{
							continue;
						}
					}
					else
					{
						if (LightData.LightCurvesSubmitted[i].XZ != LightData.LightCurvesSubmitted[i + 1].XZ || Math.Sign(LightData.LightCurvesSubmitted[i].CA) != Math.Abs(LightData.LightCurvesSubmitted[i + 1].CA))
						{
							continue;
						}
						if (Math.Abs(LightData.LightCurvesSubmitted[i].CA) < 22)
						{
							if (Math.Abs(LightData.LightCurvesSubmitted[i].PosAngle - LightData.LightCurvesSubmitted[i + 1].PosAngle) < 12.0)
							{
								continue;
							}
						}
						else if (Math.Abs(num7 - num6) > 0.0015)
						{
							continue;
						}
					}
					lstErrors.get_Items().Add((object)(i + 1));
					lstErrors.get_Items().Add((object)(i + 2));
				}
			}
			else
			{
				pBarErrors.set_Maximum(LightData.MainLightCurves.Count);
				for (int j = 0; j < LightData.MainLightCurves.Count - 1; j++)
				{
					pBarErrors.set_Value(j);
					if (LightData.MainLightCurves[j].Year != LightData.MainLightCurves[j + 1].Year || LightData.MainLightCurves[j].Month != LightData.MainLightCurves[j + 1].Month || LightData.MainLightCurves[j].Day != LightData.MainLightCurves[j + 1].Day)
					{
						continue;
					}
					double num8 = (double)LightData.MainLightCurves[j].LongD + (double)LightData.MainLightCurves[j].LongM / 60.0 + LightData.MainLightCurves[j].LongS / 3600.0;
					double num2 = (double)LightData.MainLightCurves[j + 1].LongD + (double)LightData.MainLightCurves[j + 1].LongM / 60.0 + LightData.MainLightCurves[j + 1].LongS / 3600.0;
					if (Math.Abs(num8 - num2) > 0.001)
					{
						continue;
					}
					double num9 = (double)LightData.MainLightCurves[j].LatD + (double)LightData.MainLightCurves[j].LatM / 60.0 + LightData.MainLightCurves[j].LatS / 3600.0;
					double num4 = (double)LightData.MainLightCurves[j + 1].LatD + (double)LightData.MainLightCurves[j + 1].LatM / 60.0 + LightData.MainLightCurves[j + 1].LatS / 3600.0;
					if (Math.Abs(num9 - num4) > 0.001)
					{
						continue;
					}
					string text = LightData.MainLightCurves[j].Observer.Trim().ToLower();
					int num5 = text.LastIndexOf(" ");
					if (num5 > 0)
					{
						text = text.Substring(num5);
					}
					string text2 = LightData.MainLightCurves[j + 1].Observer.Trim().ToLower();
					num5 = text2.LastIndexOf(" ");
					if (num5 > 0)
					{
						text2 = text2.Substring(num5);
					}
					if (text2 != text || LightData.MainLightCurves[j].MoonSize > 2.0 != LightData.MainLightCurves[j + 1].MoonSize > 2.0)
					{
						continue;
					}
					double num6 = (double)LightData.MainLightCurves[j].Hr + (double)LightData.MainLightCurves[j].Min / 60.0 + LightData.MainLightCurves[j].Sec / 3600.0;
					double num7 = (double)LightData.MainLightCurves[j + 1].Hr + (double)LightData.MainLightCurves[j + 1].Min / 60.0 + LightData.MainLightCurves[j + 1].Sec / 3600.0;
					if (LightData.MainLightCurves[j].MoonSize > 2.0)
					{
						if (LightData.MainLightCurves[j].AsteroidNumber != LightData.MainLightCurves[j + 1].AsteroidNumber || ((Math.Abs(num7 - num6) > 0.018) & (Math.Abs(num7 - num6) < 0.9982)) || Math.Abs(num7 - num6) > 1.0018)
						{
							continue;
						}
					}
					else
					{
						if (LightData.MainLightCurves[j].XZ != LightData.MainLightCurves[j + 1].XZ || Math.Sign(LightData.MainLightCurves[j].CA) != Math.Sign(LightData.MainLightCurves[j + 1].CA))
						{
							continue;
						}
						if (Math.Abs(LightData.MainLightCurves[j].CA) < 22)
						{
							if (Math.Abs(LightData.MainLightCurves[j].PosAngle - LightData.MainLightCurves[j + 1].PosAngle) < 12.0)
							{
								continue;
							}
						}
						else if (Math.Abs(num7 - num6) > 0.0095)
						{
							continue;
						}
					}
					lstErrors.get_Items().Add((object)(j + 1));
					lstErrors.get_Items().Add((object)(j + 2));
				}
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			((Control)pBarErrors).set_Visible(false);
			((Control)lblToEditCount).set_Text(lstErrors.get_Items().get_Count() + " curves to edit");
			((Control)lblToEditCount).set_Visible(true);
		}

		private void updnLongDeg_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongDeg).Select(0, 5);
		}

		private void updnLongMin_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongMin).Select(0, 5);
		}

		private void updnLongSec_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongSec).Select(0, 5);
		}

		private void updnLatDeg_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatDeg).Select(0, 5);
		}

		private void updnLatMin_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatMin).Select(0, 5);
		}

		private void updnLatSec_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatSec).Select(0, 5);
		}

		private void updnAlt_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAlt).Select(0, 7);
		}

		private void updnAA_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAA).Select(0, 7);
		}

		private void updnLibL_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLibL).Select(0, 7);
		}

		private void updnLibB_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLibB).Select(0, 7);
		}

		private void updnSlope_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnSlope).Select(0, 7);
		}

		private void updnLimbMotion_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLimbMotion).Select(0, 7);
		}

		private void updnContact_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnContact).Select(0, 7);
		}

		private void updnRadius_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnRadius).Select(0, 7);
		}

		private void updnPA_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnPA).Select(0, 7);
		}

		private void updnCA_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnCA).Select(0, 7);
		}

		private void updnIllum_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnIllum).Select(0, 7);
		}

		private void updnMoonAlt_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMoonAlt).Select(0, 7);
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 7);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 7);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 7);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 7);
		}

		private void updnMinute_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMinute).Select(0, 7);
		}

		private void updnSecond_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnSecond).Select(0, 7);
		}

		private void updnDuration_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDuration).Select(0, 7);
		}

		private void updnPoints_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnPoints).Select(0, 7);
		}

		private void txtObserver_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtObserver).SelectAll();
		}

		private void updnAsteroidNo_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnAsteroidNo).Select(0, 7);
		}

		private void cmdPlotAll_Click(object sender, EventArgs e)
		{
			TextBox obj = txtStart;
			string text;
			((Control)txtEnd).set_Text(text = "");
			((Control)obj).set_Text(text);
			List<int> RecordNumbersForMultiCurve = new List<int>();
			for (int i = 0; i < lstErrors.get_Items().get_Count(); i++)
			{
				RecordNumbersForMultiCurve.Add(int.Parse(lstErrors.get_Items().get_Item(i).ToString()) - 1);
			}
			if (EditSubmitted)
			{
				LightData.LightCurveView.MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: false, Submitted: true, Mine: false, Pending: false, ToDelete: false, ToReview: false, chkShowSkippedFrames.get_Checked());
			}
			else
			{
				LightData.LightCurveView.MultiCurvePlot(ref RecordNumbersForMultiCurve, Main: true, Submitted: false, Mine: false, Pending: false, ToDelete: false, ToReview: false, chkShowSkippedFrames.get_Checked());
			}
		}

		private void cmdZerovalues_Click(object sender, EventArgs e)
		{
			int num = 1000;
			((Control)lstErrors).set_BackColor(Color.LightCyan);
			lstErrors.get_Items().Clear();
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			((Control)pBarErrors).set_Visible(true);
			((Control)lblToEditCount).set_Visible(false);
			if (EditSubmitted)
			{
				pBarErrors.set_Maximum(LightData.LightCurvesSubmitted.Count);
				for (int i = 0; i < LightData.LightCurvesSubmitted.Count; i++)
				{
					pBarErrors.set_Value(i);
					Application.DoEvents();
					for (int j = 1; j < LightData.LightCurvesSubmitted[i].LightValues.Count - 1; j++)
					{
						if (((LightData.LightCurvesSubmitted[i].LightValues[j] == 0) & LightData.LightCurvesSubmitted[i].LightValuesValid[j]) && ((LightData.LightCurvesSubmitted[i].LightValues[j - 1] == 0) | (LightData.LightCurvesSubmitted[i].LightValues[j + 1] == 0) | (Math.Abs(LightData.LightCurvesSubmitted[i].LightValues[j - 1]) > num) | (Math.Abs(LightData.LightCurvesSubmitted[i].LightValues[j + 1]) > num)))
						{
							lstErrors.get_Items().Add((object)(i + 1));
							break;
						}
					}
				}
			}
			else
			{
				pBarErrors.set_Maximum(LightData.MainLightCurves.Count);
				for (int k = 0; k < LightData.MainLightCurves.Count; k++)
				{
					pBarErrors.set_Value(k);
					Application.DoEvents();
					for (int l = 1; l < LightData.MainLightCurves[k].LightValues.Count - 1; l++)
					{
						if (((LightData.MainLightCurves[k].LightValues[l] == 0) & LightData.MainLightCurves[k].LightValuesValid[l]) && ((LightData.MainLightCurves[k].LightValues[l - 1] == 0) | (LightData.MainLightCurves[k].LightValues[l + 1] == 0) | (LightData.MainLightCurves[k].LightValues[l - 1] > num) | (LightData.MainLightCurves[k].LightValues[l + 1] > num)))
						{
							lstErrors.get_Items().Add((object)(k + 1));
							break;
						}
					}
				}
			}
			((Control)this).set_Cursor(Cursors.get_Default());
			((Control)pBarErrors).set_Visible(false);
			((Control)lblToEditCount).set_Text(lstErrors.get_Items().get_Count() + " curves to edit");
			((Control)lblToEditCount).set_Visible(true);
		}

		private void cmd_x1_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(1m);
		}

		private void cmd_x2_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(2m);
		}

		private void cmd_x4_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(4m);
		}

		private void cmd_x8_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(8m);
		}

		private void cmd_x16_Click(object sender, EventArgs e)
		{
			updnScale.set_Value(16m);
		}

		private void updnScale_ValueChanged(object sender, EventArgs e)
		{
			PlotEditorLightCurve();
		}

		private void PanelPlot_Scroll(object sender, ScrollEventArgs e)
		{
			try
			{
				Application.DoEvents();
			}
			catch
			{
			}
		}

		private void picLightCurve_MouseMove(object sender, MouseEventArgs e)
		{
			if (LightData.LightCurveView != null)
			{
				if (!chkTimeHeight.get_Checked())
				{
					toolTip1.set_Active(false);
					return;
				}
				float num = e.get_X();
				float num2 = e.get_Y();
				double num3 = (double)(num / (float)((Control)picLightCurve).get_Width()) * LCDforEdit.Duration;
				double degree = ((double)(LCDforEdit.Hr * 3600 + LCDforEdit.Min * 60) + LCDforEdit.Sec + num3) / 3600.0;
				double num4 = (double)LCDforEdit.LightValues.Count / (double)((Control)picLightCurve).get_Width();
				string text = string.Format("{0,1:f0}| ", (double)num * num4);
				string text2 = "UT: " + Utilities.DEGtoDMS(degree, 1, 1, MinutesOnly: false);
				float num5 = (float)((Control)picLightCurve).get_Height() - LightData.ZeroHeight;
				float num6 = 100f * (num5 - num2) / num5;
				string text3 = string.Format(", Ht: {0,1:f0}", num6);
				toolTip1.SetToolTip((Control)(object)picLightCurve, text + text2 + text3);
			}
		}

		private void picLightCurve_MouseEnter(object sender, EventArgs e)
		{
			toolTip1.set_Active(true);
		}

		private void picLightCurve_MouseLeave(object sender, EventArgs e)
		{
		}

		private void chkShowSkippedFrames_Click(object sender, EventArgs e)
		{
			PlotEditorLightCurve();
		}

		private void cmdSetInvalid_Click(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtStart).get_Text(), out var result);
			int.TryParse(((Control)txtEnd).get_Text(), out var result2);
			for (int i = result; i <= result2; i++)
			{
				if (LCDforEdit.LightValues[i] == 0)
				{
					LCDforEdit.LightValuesValid[i] = false;
				}
			}
			PlotEditorLightCurve();
		}

		private void picLightCurve_MouseClick(object sender, MouseEventArgs e)
		{
			float num = e.get_X();
			double num2 = (double)LCDforEdit.LightValues.Count / (double)((Control)picLightCurve).get_Width();
			if (optStart.get_Checked())
			{
				((Control)txtStart).set_Text(string.Format("{0,1:f0}", (double)num * num2));
				((Control)txtSingle).set_Text("");
				optEnd.set_Checked(true);
				optStart.set_Checked(false);
			}
			else if (optEnd.get_Checked())
			{
				((Control)txtEnd).set_Text(string.Format("{0,1:f0}", (double)num * num2));
				((Control)txtSingle).set_Text("");
				optStart.set_Checked(true);
				optEnd.set_Checked(false);
			}
			else
			{
				TextBox obj = txtStart;
				TextBox obj2 = txtEnd;
				string text;
				((Control)txtSingle).set_Text(text = string.Format("{0,1:f0}", (double)num * num2));
				string text2;
				((Control)obj2).set_Text(text2 = text);
				((Control)obj).set_Text(text2);
			}
			PlotEditorLightCurve();
		}

		private void cmdResetToValid_Click(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtStart).get_Text(), out var result);
			int.TryParse(((Control)txtEnd).get_Text(), out var result2);
			for (int i = result; i < result2; i++)
			{
				if (LCDforEdit.LightValues[i] == 0)
				{
					LCDforEdit.LightValuesValid[i] = true;
				}
			}
			PlotEditorLightCurve();
		}

		private void cmdSelectAll_Click(object sender, EventArgs e)
		{
			((Control)txtStart).set_Text("0");
			((Control)txtEnd).set_Text((LCDforEdit.LightValues.Count - 1).ToString());
			PlotEditorLightCurve();
		}

		private void chkValid_CheckedChanged(object sender, EventArgs e)
		{
			Button obj = cmdZerovalues;
			bool @checked;
			((Control)panelSetInvalid).set_Visible(@checked = chkValid.get_Checked());
			((Control)obj).set_Visible(@checked);
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Light Curve Editor");
		}

		private void cmd_csvfile_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("Tangra       ");
			stringBuilder.AppendLine("Date " + updnYear.get_Value() + " " + Utilities.ShortMonths[(int)updnMonth.get_Value()] + " " + updnDay.get_Value());
			if (updnU4a.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = UCAC4 " + updnU4a.get_Value() + "-" + updnU4b.get_Value());
			}
			else if (updnTa.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = TYC2 " + updnTa.get_Value() + "-" + updnTb.get_Value() + "-" + updnTc.get_Value());
			}
			else if (updnHip.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = Hip2 " + updnHip.get_Value());
			}
			else if (updnZC.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = ZC " + updnZC.get_Value());
			}
			else if (updnSAO.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = SAO " + updnSAO.get_Value());
			}
			else if (updnXZ.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = XZ " + updnXZ.get_Value());
			}
			if (optAsteroid.get_Checked())
			{
				stringBuilder.AppendLine("Asteroid = (" + updnAsteroidNo.get_Value() + ") " + ((Control)txtAsteroidName).get_Text());
			}
			stringBuilder.AppendLine("Observer = " + ((Control)txtObserver).get_Text());
			stringBuilder.AppendLine("FrameNo,Time (UT),Signal (1), Background (1)");
			double num = (double)updnHour.get_Value() * 3600.0 + (double)updnMinute.get_Value() * 60.0 + (double)updnSecond.get_Value();
			double num2 = (double)updnDuration.get_Value();
			double num3 = (double)updnPoints.get_Value();
			double num4 = num2 / (num3 - 1.0);
			for (int i = 0; i < LCDforEdit.LightValues.Count; i++)
			{
				double num5 = num + (double)i * num4;
				stringBuilder.AppendLine(i + ",[" + Utilities.DEGtoDMS(num5 / 3600.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true).Trim().Replace(" ", ":") + "]," + LCDforEdit.LightValues[i] + ",0");
			}
			try
			{
				((Control)AOTAData.PlotForm).Show();
			}
			catch
			{
				AOTAData.PlotForm = new PlotForm();
				((Form)this).set_StartPosition((FormStartPosition)4);
				((Control)AOTAData.PlotForm).Show();
			}
			AOTAData.Read_CSV_Data(stringBuilder.ToString(), FromLightCurve: false, FrameInsertions_Allow: false);
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated Files\\csv_from_LightCurve.csv");
			streamWriter.Write(stringBuilder.ToString());
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picLightCurve.get_Image());
		}

		private void displayEventInAOTAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("Tangra       ");
			stringBuilder.AppendLine("Date " + updnYear.get_Value() + " " + Utilities.ShortMonths[(int)updnMonth.get_Value()] + " " + updnDay.get_Value());
			if (updnU4a.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = UCAC4 " + updnU4a.get_Value() + "-" + updnU4b.get_Value());
			}
			else if (updnTa.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = TYC2 " + updnTa.get_Value() + "-" + updnTb.get_Value() + "-" + updnTc.get_Value());
			}
			else if (updnHip.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = Hip2 " + updnHip.get_Value());
			}
			else if (updnZC.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = ZC " + updnZC.get_Value());
			}
			else if (updnSAO.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = SAO " + updnSAO.get_Value());
			}
			else if (updnXZ.get_Value() > 0m)
			{
				stringBuilder.AppendLine("Star = XZ " + updnXZ.get_Value());
			}
			if (optAsteroid.get_Checked())
			{
				stringBuilder.AppendLine("Asteroid = (" + updnAsteroidNo.get_Value() + ") " + ((Control)txtAsteroidName).get_Text());
			}
			stringBuilder.AppendLine("Observer = " + ((Control)txtObserver).get_Text());
			stringBuilder.AppendLine("FrameNo,Time (UT),Signal (1), Background (1)");
			double num = (double)updnHour.get_Value() * 3600.0 + (double)updnMinute.get_Value() * 60.0 + (double)updnSecond.get_Value();
			double num2 = (double)updnDuration.get_Value();
			double num3 = (double)updnPoints.get_Value();
			double num4 = num2 / (num3 - 1.0);
			for (int i = 0; i < LCDforEdit.LightValues.Count; i++)
			{
				double num5 = num + (double)i * num4;
				stringBuilder.AppendLine(i + ",[" + Utilities.DEGtoDMS(num5 / 3600.0, 2, 3, MinutesOnly: false, IncludeLeadingZeros: true).Trim().Replace(" ", ":") + "]," + LCDforEdit.LightValues[i] + ",0");
			}
			try
			{
				((Control)AOTAData.PlotForm).Show();
			}
			catch
			{
				AOTAData.PlotForm = new PlotForm();
				((Form)this).set_StartPosition((FormStartPosition)4);
				((Control)AOTAData.PlotForm).Show();
			}
			AOTAData.Read_CSV_Data(stringBuilder.ToString(), FromLightCurve: false, FrameInsertions_Allow: false);
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Generated Files\\csv_from_LightCurve.csv");
			streamWriter.Write(stringBuilder.ToString());
		}

		private void exitSaveChangesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008b: Invalid comparison between Unknown and I4
			if (EditSubmitted)
			{
				((Form)this).Close();
				return;
			}
			if (MainLightCurvesEdited > 0 && (int)MessageBox.Show("Do you want to save the edited main observations", "Save edited", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				int num = 0;
				bool removeExcluded = false;
				for (int i = 0; i < LightData.MainLightCurves.Count; i++)
				{
					if (LightData.MainLightCurves[i].ExcludeLightCurve)
					{
						num++;
					}
				}
				if (num > 0 && (int)MessageBox.Show(num + " light curves are flagged to be excluded\r\n\r\nIf they are to be excluded, this must happen with this Save\r\nOtherwise the flag will be removed\r\n\r\nDo you want to excluded flagged events?", "Exclude flagged events", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					removeExcluded = true;
				}
				LightData.SaveMainLightCurves(removeExcluded);
			}
			((Form)this).Close();
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.cmdCancel).set_Visible(false);
			((Control)messageForm).Show();
			((Control)messageForm.label).set_Text("Reload all light curves");
			Application.DoEvents();
			Cursor.set_Current(Cursors.get_WaitCursor());
			LightData.LightCurveView.ReadLightCurves();
			Cursor.set_Current(Cursors.get_Default());
			((Form)messageForm).Close();
		}

		private void copyLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				Clipboard.SetImage(picLightCurve.get_Image());
			}
			catch
			{
				MessageBox.Show("Error - Image not copied");
			}
		}

		private void updnFirst_ValueChanged(object sender, EventArgs e)
		{
			if (((Control)pnlIntegration).get_Visible())
			{
				PlotEditorLightCurve();
			}
		}

		private void updnFramesToCombine_ValueChanged(object sender, EventArgs e)
		{
			if (((Control)pnlIntegration).get_Visible())
			{
				PlotEditorLightCurve();
			}
		}

		private void cmdIntegrate_Click(object sender, EventArgs e)
		{
			//IL_035a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Unknown result type (might be due to invalid IL or missing references)
			((Control)pnlIntegration).set_Visible(false);
			OriginalLightValues = new List<int>();
			OriginalLightValid = new List<bool>();
			for (int i = 0; i < LCDforEdit.LightValues.Count; i++)
			{
				OriginalLightValues.Add(LCDforEdit.LightValues[i]);
				OriginalLightValid.Add(LCDforEdit.LightValuesValid[i]);
			}
			OriginalNumberOfPoints = updnPoints.get_Value();
			OriginalDuration = updnDuration.get_Value();
			OriginalSeconds = updnSecond.get_Value();
			OriginalMinutes = updnMinute.get_Value();
			OriginalHours = updnHour.get_Value();
			int num = (int)updnFirst.get_Value();
			int num2 = (int)updnFramesToCombine.get_Value();
			List<int> list = new List<int>();
			for (int j = num; j < LCDforEdit.LightValues.Count - num2; j += num2)
			{
				int num3 = 0;
				for (int k = 0; k < num2; k++)
				{
					num3 += LCDforEdit.LightValues[j + k];
				}
				num3 /= num2;
				list.Add(num3);
			}
			double num4 = (double)updnDuration.get_Value() / ((double)updnPoints.get_Value() - 1.0);
			double num5 = num4 * (double)num2;
			updnDuration.set_Value((decimal)((double)Convert.ToInt32(100.0 * (num5 * (double)(list.Count - 1))) / 100.0));
			updnPoints.set_Value((decimal)list.Count);
			double num6 = (double)Convert.ToInt32(100.0 * ((double)updnSecond.get_Value() + (double)num * num4)) / 100.0;
			if (num6 < 60.0)
			{
				updnSecond.set_Value((decimal)num6);
			}
			else
			{
				updnSecond.set_Value((decimal)num6 - 60m);
				decimal num7 = updnMinute.get_Value() + 1m;
				if (num7 < 60m)
				{
					updnMinute.set_Value(num7);
				}
				else
				{
					NumericUpDown obj = updnHour;
					obj.set_Value(obj.get_Value() + 1m);
				}
			}
			LCDforEdit.LightValues.Clear();
			LCDforEdit.LightValuesValid.Clear();
			for (int l = 0; l < list.Count; l++)
			{
				LCDforEdit.LightValues.Add(list[l]);
				LCDforEdit.LightValuesValid.Add(item: true);
			}
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Enabled(false);
			updnFirst.set_Value(0m);
			updnFramesToCombine.set_Value(1m);
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Enabled(false);
			EncodeRecordBeingEdited();
			UpdatingIntegration = false;
			if (!EditSubmitted)
			{
				MessageBox.Show("This light curve is in the Main file of light curves\r\n\r\nThe Light curve file will not be updated when the Edit \r\nform is closed unless the green \r\n      Main light curves: Accept... \r\nbutton has been clicked for at least one of the \r\nlight curves that have been edited.\r\n", "Saving the edited light curve", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				MessageBox.Show("This light curve is a Submitted file of light curves\r\n\r\nTo update that file with the edited light curve\r\nthe green \r\n      Submitted light curves : Update... \r\nmust be clicked before editing another light curve\r\nor leaving the editor.\r\n", "Saving the edited light curve", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			PlotEditorLightCurve();
		}

		private void ResetIntegrationMenuEnabling()
		{
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Enabled(true);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Control)pnlIntegration).set_Visible(false);
			UndoIntegrationChanges();
			PlotEditorLightCurve();
		}

		private void binIntegrationblocksToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			((Control)pnlIntegration).set_Visible(true);
			if (EditSubmitted)
			{
				((Control)pnlIntegration).set_Height(134);
			}
			else
			{
				((Control)pnlIntegration).set_Height(362);
			}
			((Control)pnlIntegration).set_Top(((Control)PanelPlot).get_Top() - ((Control)pnlIntegration).get_Height() - 5);
			OriginalLightValues.Clear();
		}

		private void UndoIntegrationChanges()
		{
			((Control)pnlIntegration).set_Visible(false);
			UpdatingIntegration = true;
			if (OriginalLightValues.Count > 0)
			{
				LCDforEdit.LightValues.Clear();
				LCDforEdit.LightValuesValid.Clear();
				for (int i = 0; i < OriginalLightValues.Count; i++)
				{
					LCDforEdit.LightValues.Add(OriginalLightValues[i]);
					LCDforEdit.LightValuesValid.Add(OriginalLightValid[i]);
				}
				updnPoints.set_Value(OriginalNumberOfPoints);
				updnDuration.set_Value(OriginalDuration);
				updnSecond.set_Value(OriginalSeconds);
				updnMinute.set_Value(OriginalMinutes);
				updnHour.set_Value(OriginalHours);
			}
			ResetIntegrationMenuEnabling();
			EncodeRecordBeingEdited();
			UpdatingIntegration = false;
			PlotEditorLightCurve();
		}

		private void undoIntegrationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			UndoIntegrationChanges();
		}

		private void integrationToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void cmdTruncate_Click(object sender, EventArgs e)
		{
			//IL_0385: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a0: Unknown result type (might be due to invalid IL or missing references)
			((Control)pnlTruncate).set_Visible(false);
			OriginalLightValues = new List<int>();
			OriginalLightValid = new List<bool>();
			for (int i = 0; i < LCDforEdit.LightValues.Count; i++)
			{
				OriginalLightValues.Add(LCDforEdit.LightValues[i]);
				OriginalLightValid.Add(LCDforEdit.LightValuesValid[i]);
			}
			OriginalNumberOfPoints = updnPoints.get_Value();
			OriginalDuration = updnDuration.get_Value();
			OriginalSeconds = updnSecond.get_Value();
			OriginalMinutes = updnMinute.get_Value();
			OriginalHours = updnHour.get_Value();
			int num = (int)updnFirstFrame.get_Value();
			int num2 = (int)updnLastFrame.get_Value() - 1;
			List<int> list = new List<int>();
			for (int j = num; j <= num2; j++)
			{
				list.Add(LCDforEdit.LightValues[j]);
			}
			double num3 = (double)updnDuration.get_Value() / ((double)updnPoints.get_Value() - 1.0);
			updnPoints.set_Value((decimal)list.Count);
			updnDuration.set_Value((decimal)((double)Convert.ToInt32(100.0 * (num3 * (double)(list.Count - 1))) / 100.0));
			double num4 = 3600.0 * (double)updnHour.get_Value() + 60.0 * (double)updnMinute.get_Value() + (double)updnSecond.get_Value();
			double num5 = (double)Convert.ToInt32(100.0 * (num4 + (double)num * num3)) / 100.0;
			int num6 = (int)(num5 / 3600.0);
			int num7 = (int)((num5 - (double)(3600 * num6)) / 60.0);
			double num8 = (double)Convert.ToInt32((num5 - (double)(3600 * num6) - (double)(60 * num7)) * 100.0) / 100.0;
			if (num8 > 59.99)
			{
				num8 = 59.99;
			}
			if (num8 < 0.0)
			{
				num8 = 0.0;
			}
			updnHour.set_Value((decimal)num6);
			updnMinute.set_Value((decimal)num7);
			updnSecond.set_Value((decimal)num8);
			LCDforEdit.LightValues.Clear();
			LCDforEdit.LightValuesValid.Clear();
			for (int k = 0; k < list.Count; k++)
			{
				LCDforEdit.LightValues.Add(list[k]);
				LCDforEdit.LightValuesValid.Add(item: true);
			}
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)truncateToolStripMenuItem).set_Enabled(false);
			updnFirst.set_Value(0m);
			updnFramesToCombine.set_Value(1m);
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)truncateToolStripMenuItem).set_Enabled(false);
			EncodeRecordBeingEdited();
			UpdatingTruncation = false;
			if (!EditSubmitted)
			{
				MessageBox.Show("This light curve is in the Main file of light curves\r\n\r\nThe Light curve file will not be updated when the Edit \r\nform is closed unless the green \r\n      Main light curves: Accept... \r\nbutton has been clicked for at least one of the \r\nlight curves that have been edited.\r\n", "Saving the edited light curve", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			else
			{
				MessageBox.Show("This light curve is a Submitted file of light curves\r\n\r\nTo update that file with the edited light curve\r\nthe green \r\n      Submitted light curves : Update... \r\nmust be clicked before editing another light curve\r\nor leaving the editor.\r\n", "Saving the edited light curve", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			PlotEditorLightCurve();
		}

		private void truncateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)pnlTruncate).set_Visible(true);
			((Control)pnlTruncate).set_Left(((Control)pnlIntegration).get_Left());
			if (EditSubmitted)
			{
				((Control)pnlTruncate).set_Height(134);
			}
			else
			{
				((Control)pnlTruncate).set_Height(362);
			}
			((Control)pnlTruncate).set_Top(((Control)PanelPlot).get_Top() - ((Control)pnlTruncate).get_Height() - 5);
			NumericUpDown obj = updnLastFrame;
			decimal value;
			updnLastFrame.set_Maximum(value = updnPoints.get_Value());
			obj.set_Value(value);
			decimal num = updnPoints.get_Value() - 100m;
			if (num < 0m)
			{
				num = default(decimal);
			}
			updnFirstFrame.set_Maximum(num);
			updnFirstFrame.set_Value(0m);
			OriginalLightValues.Clear();
		}

		private void undoTruncationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			UndoTruncationChanges();
		}

		private void UndoTruncationChanges()
		{
			UpdatingTruncation = true;
			((Control)pnlTruncate).set_Visible(false);
			if (OriginalLightValues.Count > 0)
			{
				LCDforEdit.LightValues.Clear();
				LCDforEdit.LightValuesValid.Clear();
				for (int i = 0; i < OriginalLightValues.Count; i++)
				{
					LCDforEdit.LightValues.Add(OriginalLightValues[i]);
					LCDforEdit.LightValuesValid.Add(OriginalLightValid[i]);
				}
				updnPoints.set_Value(OriginalNumberOfPoints);
				updnDuration.set_Value(OriginalDuration);
				updnSecond.set_Value(OriginalSeconds);
				updnMinute.set_Value(OriginalMinutes);
				updnHour.set_Value(OriginalHours);
			}
			ResetTruncationMenuEnabling();
			EncodeRecordBeingEdited();
			UpdatingTruncation = false;
			PlotEditorLightCurve();
		}

		private void ResetTruncationMenuEnabling()
		{
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)truncateToolStripMenuItem).set_Enabled(true);
		}

		private void updnFirstFrame_ValueChanged(object sender, EventArgs e)
		{
			if (((Control)pnlTruncate).get_Visible())
			{
				PlotEditorLightCurve();
			}
		}

		private void updnLastFrame_ValueChanged(object sender, EventArgs e)
		{
			if (((Control)pnlTruncate).get_Visible())
			{
				PlotEditorLightCurve();
			}
		}

		private void cmdCancelTruncate_Click(object sender, EventArgs e)
		{
			UndoTruncationChanges();
		}

		private void invalidFramesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			invalidFramesToolStripMenuItem.set_Checked(!invalidFramesToolStripMenuItem.get_Checked());
			if (!invalidFramesToolStripMenuItem.get_Checked())
			{
				((ToolStripItem)invalidFramesToolStripMenuItem).set_Text("Invalid frame editor - make visible");
			}
			else
			{
				((ToolStripItem)invalidFramesToolStripMenuItem).set_Text("Invalid frame editor - hide");
			}
			Panel obj = panelSetInvalid;
			bool @checked;
			chkValid.set_Checked(@checked = invalidFramesToolStripMenuItem.get_Checked());
			((Control)obj).set_Visible(@checked);
		}

		private void optSingle_Click(object sender, EventArgs e)
		{
			optSingle.set_Checked(!optSingle.get_Checked());
			if (optSingle.get_Checked())
			{
				RadioButton obj = optStart;
				bool @checked;
				optEnd.set_Checked(@checked = false);
				obj.set_Checked(@checked);
			}
		}

		private void optStart_Click(object sender, EventArgs e)
		{
			optStart.set_Checked(true);
			optSingle.set_Checked(false);
			if (optStart.get_Checked())
			{
				RadioButton obj = optSingle;
				bool @checked;
				optEnd.set_Checked(@checked = false);
				obj.set_Checked(@checked);
			}
		}

		private void optEnd_Click(object sender, EventArgs e)
		{
			optEnd.set_Checked(true);
			optSingle.set_Checked(false);
			if (optStart.get_Checked())
			{
				optStart.set_Checked(false);
			}
		}

		private void cmdSetFrameInvalid_Click(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtSingle).get_Text(), out var result);
			LCDforEdit.LightValuesValid[result] = false;
			PlotEditorLightCurve();
		}

		private void cmdSetFrameValid_Click(object sender, EventArgs e)
		{
			int.TryParse(((Control)txtSingle).get_Text(), out var result);
			LCDforEdit.LightValuesValid[result] = true;
			PlotEditorLightCurve();
		}

		private void updnLightCurveNumber_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				Edit();
			}
		}

		private void updnLightCurveNumber_ValueChanged(object sender, EventArgs e)
		{
			Edit();
		}

		private void swapLatitudeLongitudeValuesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			decimal num = default(decimal);
			text = ((Control)txtNS).get_Text();
			((Control)txtNS).set_Text(((Control)txtEW).get_Text());
			((Control)txtEW).set_Text(text);
			num = updnLongDeg.get_Value();
			updnLongDeg.set_Value(updnLatDeg.get_Value());
			updnLatDeg.set_Value(num);
			num = updnLongMin.get_Value();
			updnLongMin.set_Value(updnLatMin.get_Value());
			updnLatMin.set_Value(num);
			num = updnLongSec.get_Value();
			updnLongSec.set_Value(updnLatSec.get_Value());
			updnLatSec.set_Value(num);
		}

		private void flipSignOfLongitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)txtEW).get_Text() == "+")
			{
				((Control)txtEW).set_Text("-");
			}
			else
			{
				((Control)txtEW).set_Text("+");
			}
		}

		private void flipSignOfLatitudeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)txtNS).get_Text() == "+")
			{
				((Control)txtNS).set_Text("-");
			}
			else
			{
				((Control)txtNS).set_Text("+");
			}
		}

		private void cmdCancelCheck_Click(object sender, EventArgs e)
		{
			CancelCheck = true;
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
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_067e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0688: Expected O, but got Unknown
			//IL_0689: Unknown result type (might be due to invalid IL or missing references)
			//IL_0693: Expected O, but got Unknown
			//IL_0694: Unknown result type (might be due to invalid IL or missing references)
			//IL_069e: Expected O, but got Unknown
			//IL_069f: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a9: Expected O, but got Unknown
			//IL_06aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b4: Expected O, but got Unknown
			//IL_06b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06bf: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_06cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d5: Expected O, but got Unknown
			//IL_06d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e0: Expected O, but got Unknown
			//IL_06e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06eb: Expected O, but got Unknown
			//IL_06ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f6: Expected O, but got Unknown
			//IL_06f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0701: Expected O, but got Unknown
			//IL_0702: Unknown result type (might be due to invalid IL or missing references)
			//IL_070c: Expected O, but got Unknown
			//IL_070d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0717: Expected O, but got Unknown
			//IL_0718: Unknown result type (might be due to invalid IL or missing references)
			//IL_0722: Expected O, but got Unknown
			//IL_0723: Unknown result type (might be due to invalid IL or missing references)
			//IL_072d: Expected O, but got Unknown
			//IL_072e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0738: Expected O, but got Unknown
			//IL_0739: Unknown result type (might be due to invalid IL or missing references)
			//IL_0743: Expected O, but got Unknown
			//IL_0744: Unknown result type (might be due to invalid IL or missing references)
			//IL_074e: Expected O, but got Unknown
			//IL_074f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0759: Expected O, but got Unknown
			//IL_075a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0764: Expected O, but got Unknown
			//IL_0765: Unknown result type (might be due to invalid IL or missing references)
			//IL_076f: Expected O, but got Unknown
			//IL_0770: Unknown result type (might be due to invalid IL or missing references)
			//IL_077a: Expected O, but got Unknown
			//IL_077b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0785: Expected O, but got Unknown
			//IL_0786: Unknown result type (might be due to invalid IL or missing references)
			//IL_0790: Expected O, but got Unknown
			//IL_0791: Unknown result type (might be due to invalid IL or missing references)
			//IL_079b: Expected O, but got Unknown
			//IL_079c: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a6: Expected O, but got Unknown
			//IL_07a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b1: Expected O, but got Unknown
			//IL_07b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07bc: Expected O, but got Unknown
			//IL_07bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c7: Expected O, but got Unknown
			//IL_07c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d2: Expected O, but got Unknown
			//IL_07d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07dd: Expected O, but got Unknown
			//IL_07de: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e8: Expected O, but got Unknown
			//IL_07e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f3: Expected O, but got Unknown
			//IL_07f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fe: Expected O, but got Unknown
			//IL_07ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0809: Expected O, but got Unknown
			//IL_080a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0814: Expected O, but got Unknown
			//IL_0815: Unknown result type (might be due to invalid IL or missing references)
			//IL_081f: Expected O, but got Unknown
			//IL_0820: Unknown result type (might be due to invalid IL or missing references)
			//IL_082a: Expected O, but got Unknown
			//IL_082b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0835: Expected O, but got Unknown
			//IL_0836: Unknown result type (might be due to invalid IL or missing references)
			//IL_0840: Expected O, but got Unknown
			//IL_0841: Unknown result type (might be due to invalid IL or missing references)
			//IL_084b: Expected O, but got Unknown
			//IL_084c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0856: Expected O, but got Unknown
			//IL_0857: Unknown result type (might be due to invalid IL or missing references)
			//IL_0861: Expected O, but got Unknown
			//IL_0862: Unknown result type (might be due to invalid IL or missing references)
			//IL_086c: Expected O, but got Unknown
			//IL_086d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0877: Expected O, but got Unknown
			//IL_0878: Unknown result type (might be due to invalid IL or missing references)
			//IL_0882: Expected O, but got Unknown
			//IL_0883: Unknown result type (might be due to invalid IL or missing references)
			//IL_088d: Expected O, but got Unknown
			//IL_088e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0898: Expected O, but got Unknown
			//IL_0899: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a3: Expected O, but got Unknown
			//IL_08a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ae: Expected O, but got Unknown
			//IL_08af: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b9: Expected O, but got Unknown
			//IL_08ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c4: Expected O, but got Unknown
			//IL_08c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08cf: Expected O, but got Unknown
			//IL_08d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08da: Expected O, but got Unknown
			//IL_08db: Unknown result type (might be due to invalid IL or missing references)
			//IL_08e5: Expected O, but got Unknown
			//IL_08e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f0: Expected O, but got Unknown
			//IL_08f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fb: Expected O, but got Unknown
			//IL_08fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0906: Expected O, but got Unknown
			//IL_0907: Unknown result type (might be due to invalid IL or missing references)
			//IL_0911: Expected O, but got Unknown
			//IL_0912: Unknown result type (might be due to invalid IL or missing references)
			//IL_091c: Expected O, but got Unknown
			//IL_091d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0927: Expected O, but got Unknown
			//IL_0928: Unknown result type (might be due to invalid IL or missing references)
			//IL_0932: Expected O, but got Unknown
			//IL_0cb1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cbb: Expected O, but got Unknown
			//IL_1560: Unknown result type (might be due to invalid IL or missing references)
			//IL_156a: Expected O, but got Unknown
			//IL_5cd8: Unknown result type (might be due to invalid IL or missing references)
			//IL_5ce2: Expected O, but got Unknown
			//IL_5d06: Unknown result type (might be due to invalid IL or missing references)
			//IL_5d10: Expected O, but got Unknown
			//IL_5e1b: Unknown result type (might be due to invalid IL or missing references)
			//IL_5e25: Expected O, but got Unknown
			//IL_7eeb: Unknown result type (might be due to invalid IL or missing references)
			//IL_7ef5: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(LightCurve_Edit));
			updnLightCurveNumber = new NumericUpDown();
			lblSource = new Label();
			panel1 = new Panel();
			cmdCancelCheck = new Button();
			chkEmbargoed = new CheckBox();
			cmdPlotAll = new Button();
			label34 = new Label();
			label33 = new Label();
			label32 = new Label();
			cmdDuplicates = new Button();
			cmdCheckSAO_XZ = new Button();
			label2 = new Label();
			lblToEditCount = new Label();
			pBarErrors = new ProgressBar();
			label1 = new Label();
			lstErrors = new ListBox();
			cmdCheckForErrors = new Button();
			label35 = new Label();
			label36 = new Label();
			lblMainUpdated = new Label();
			cmdEdit = new Button();
			cmdZerovalues = new Button();
			updnYear = new NumericUpDown();
			groupBox1 = new GroupBox();
			lblDate = new Label();
			label12 = new Label();
			label13 = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			label17 = new Label();
			updnMonth = new NumericUpDown();
			updnSecond = new NumericUpDown();
			updnHour = new NumericUpDown();
			updnMinute = new NumericUpDown();
			updnDay = new NumericUpDown();
			panelSetInvalid = new Panel();
			label70 = new Label();
			panel8 = new Panel();
			cmdSetFrameValid = new Button();
			cmdSetFrameInvalid = new Button();
			txtSingle = new TextBox();
			optSingle = new RadioButton();
			panel5 = new Panel();
			cmdSelectAll = new Button();
			txtEnd = new TextBox();
			txtStart = new TextBox();
			optEnd = new RadioButton();
			optStart = new RadioButton();
			cmdSetInvalid = new Button();
			cmdResetToValid = new Button();
			label55 = new Label();
			label54 = new Label();
			label53 = new Label();
			label52 = new Label();
			label51 = new Label();
			label50 = new Label();
			label38 = new Label();
			lblDurn = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			groupBox2 = new GroupBox();
			lblLight = new Label();
			updnPoints = new NumericUpDown();
			updnDuration = new NumericUpDown();
			groupBox3 = new GroupBox();
			lblObserver = new Label();
			txtNS = new TextBox();
			txtEW = new TextBox();
			label25 = new Label();
			label24 = new Label();
			updnAlt = new NumericUpDown();
			label23 = new Label();
			label20 = new Label();
			label21 = new Label();
			label22 = new Label();
			updnLatSec = new NumericUpDown();
			updnLatDeg = new NumericUpDown();
			updnLatMin = new NumericUpDown();
			label3 = new Label();
			label18 = new Label();
			label19 = new Label();
			updnLongSec = new NumericUpDown();
			updnLongDeg = new NumericUpDown();
			updnLongMin = new NumericUpDown();
			txtObserver = new TextBox();
			groupBox4 = new GroupBox();
			cmdUpdateSAO = new Button();
			cmdUpdateXZ = new Button();
			lblStarLine = new Label();
			updnTb = new NumericUpDown();
			updnU4b = new NumericUpDown();
			updnTa = new NumericUpDown();
			updnTc = new NumericUpDown();
			updnU4a = new NumericUpDown();
			updnSAO = new NumericUpDown();
			updnXZ = new NumericUpDown();
			updnZC = new NumericUpDown();
			updnHip = new NumericUpDown();
			label26 = new Label();
			label31 = new Label();
			label30 = new Label();
			label27 = new Label();
			label39 = new Label();
			groupAsteroid = new GroupBox();
			cmdGetName = new Button();
			updnAsteroidNo = new NumericUpDown();
			txtAsteroidName = new TextBox();
			lblAsteroid = new Label();
			grpMoon = new GroupBox();
			cmbNS = new ComboBox();
			label48 = new Label();
			label47 = new Label();
			label46 = new Label();
			label45 = new Label();
			label44 = new Label();
			label43 = new Label();
			label42 = new Label();
			label41 = new Label();
			label40 = new Label();
			label29 = new Label();
			label28 = new Label();
			updnLibL = new NumericUpDown();
			updnLibB = new NumericUpDown();
			updnSlope = new NumericUpDown();
			updnContact = new NumericUpDown();
			lblMoon = new Label();
			updnRadius = new NumericUpDown();
			updnMoonAlt = new NumericUpDown();
			updnIllum = new NumericUpDown();
			updnCA = new NumericUpDown();
			updnPA = new NumericUpDown();
			updnLimbMotion = new NumericUpDown();
			updnAA = new NumericUpDown();
			optMoon = new RadioButton();
			optAsteroid = new RadioButton();
			groupBox5 = new GroupBox();
			chkExclude = new CheckBox();
			cmdUpdateSubmitted = new Button();
			cmdAcceptChanges = new Button();
			lblSubmittedAdvice = new Label();
			lblMainAdvice = new Label();
			toolTip1 = new ToolTip(components);
			picLightCurve = new PictureBox();
			picCheck = new PictureBox();
			PanelPlot = new Panel();
			panel3 = new Panel();
			cmd_x16 = new Button();
			chkShowSkippedFrames = new CheckBox();
			chkTimeHeight = new CheckBox();
			label37 = new Label();
			cmd_x8 = new Button();
			cmd_x4 = new Button();
			cmd_x2 = new Button();
			cmd_x1 = new Button();
			updnScale = new NumericUpDown();
			chkValid = new CheckBox();
			panel2 = new Panel();
			label49 = new Label();
			panel4 = new Panel();
			cmdHelp = new Button();
			chkReview = new CheckBox();
			menuStrip1 = new MenuStrip();
			copyLightCurveToolStripMenuItem = new ToolStripMenuItem();
			lightCurveEditingToolStripMenuItem = new ToolStripMenuItem();
			integrationToolStripMenuItem = new ToolStripMenuItem();
			binIntegrationblocksToolStripMenuItem1 = new ToolStripMenuItem();
			undoIntegrationToolStripMenuItem = new ToolStripMenuItem();
			truncationToolStripMenuItem = new ToolStripMenuItem();
			truncateToolStripMenuItem = new ToolStripMenuItem();
			undoTruncationToolStripMenuItem = new ToolStripMenuItem();
			invalidFramesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			siteEditsToolStripMenuItem = new ToolStripMenuItem();
			swapLatitudeLongitudeValuesToolStripMenuItem = new ToolStripMenuItem();
			flipSignOfLongitudeToolStripMenuItem = new ToolStripMenuItem();
			flipSignOfLatitudeToolStripMenuItem = new ToolStripMenuItem();
			displayEventInAOTAToolStripMenuItem = new ToolStripMenuItem();
			exitSaveChangesToolStripMenuItem = new ToolStripMenuItem();
			pnlIntegration = new Panel();
			panel6 = new Panel();
			label62 = new Label();
			label60 = new Label();
			label61 = new Label();
			label59 = new Label();
			cmdIntegrate = new Button();
			cmdCancel = new Button();
			label58 = new Label();
			updnFramesToCombine = new NumericUpDown();
			label57 = new Label();
			updnFirst = new NumericUpDown();
			label56 = new Label();
			pnlTruncate = new Panel();
			panel7 = new Panel();
			label67 = new Label();
			label68 = new Label();
			label69 = new Label();
			cmdTruncate = new Button();
			cmdCancelTruncate = new Button();
			label65 = new Label();
			updnLastFrame = new NumericUpDown();
			label66 = new Label();
			updnFirstFrame = new NumericUpDown();
			label64 = new Label();
			label63 = new Label();
			((ISupportInitialize)updnLightCurveNumber).BeginInit();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnSecond).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((Control)panelSetInvalid).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((ISupportInitialize)updnPoints).BeginInit();
			((ISupportInitialize)updnDuration).BeginInit();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)updnAlt).BeginInit();
			((ISupportInitialize)updnLatSec).BeginInit();
			((ISupportInitialize)updnLatDeg).BeginInit();
			((ISupportInitialize)updnLatMin).BeginInit();
			((ISupportInitialize)updnLongSec).BeginInit();
			((ISupportInitialize)updnLongDeg).BeginInit();
			((ISupportInitialize)updnLongMin).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((ISupportInitialize)updnTb).BeginInit();
			((ISupportInitialize)updnU4b).BeginInit();
			((ISupportInitialize)updnTa).BeginInit();
			((ISupportInitialize)updnTc).BeginInit();
			((ISupportInitialize)updnU4a).BeginInit();
			((ISupportInitialize)updnSAO).BeginInit();
			((ISupportInitialize)updnXZ).BeginInit();
			((ISupportInitialize)updnZC).BeginInit();
			((ISupportInitialize)updnHip).BeginInit();
			((Control)groupAsteroid).SuspendLayout();
			((ISupportInitialize)updnAsteroidNo).BeginInit();
			((Control)grpMoon).SuspendLayout();
			((ISupportInitialize)updnLibL).BeginInit();
			((ISupportInitialize)updnLibB).BeginInit();
			((ISupportInitialize)updnSlope).BeginInit();
			((ISupportInitialize)updnContact).BeginInit();
			((ISupportInitialize)updnRadius).BeginInit();
			((ISupportInitialize)updnMoonAlt).BeginInit();
			((ISupportInitialize)updnIllum).BeginInit();
			((ISupportInitialize)updnCA).BeginInit();
			((ISupportInitialize)updnPA).BeginInit();
			((ISupportInitialize)updnLimbMotion).BeginInit();
			((ISupportInitialize)updnAA).BeginInit();
			((Control)groupBox5).SuspendLayout();
			((ISupportInitialize)picLightCurve).BeginInit();
			((ISupportInitialize)picCheck).BeginInit();
			((Control)PanelPlot).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnScale).BeginInit();
			((Control)panel2).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)pnlIntegration).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((ISupportInitialize)updnFramesToCombine).BeginInit();
			((ISupportInitialize)updnFirst).BeginInit();
			((Control)pnlTruncate).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((ISupportInitialize)updnLastFrame).BeginInit();
			((ISupportInitialize)updnFirstFrame).BeginInit();
			((Control)this).SuspendLayout();
			((Control)updnLightCurveNumber).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnLightCurveNumber).set_Location(new Point(16, 28));
			updnLightCurveNumber.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLightCurveNumber).set_Name("updnLightCurveNumber");
			((Control)updnLightCurveNumber).set_Size(new Size(66, 23));
			((Control)updnLightCurveNumber).set_TabIndex(8);
			updnLightCurveNumber.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnLightCurveNumber.add_ValueChanged((EventHandler)updnLightCurveNumber_ValueChanged);
			((Control)updnLightCurveNumber).add_KeyDown(new KeyEventHandler(updnLightCurveNumber_KeyDown));
			((Control)lblSource).set_AutoSize(true);
			((Control)lblSource).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSource).set_Location(new Point(302, 40));
			((Control)lblSource).set_Name("lblSource");
			((Control)lblSource).set_Size(new Size(204, 20));
			((Control)lblSource).set_TabIndex(0);
			((Control)lblSource).set_Text("Editing Main light curves");
			((Control)panel1).set_BackColor(Color.AliceBlue);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)cmdCancelCheck);
			((Control)panel1).get_Controls().Add((Control)(object)chkEmbargoed);
			((Control)panel1).get_Controls().Add((Control)(object)cmdPlotAll);
			((Control)panel1).get_Controls().Add((Control)(object)label34);
			((Control)panel1).get_Controls().Add((Control)(object)label33);
			((Control)panel1).get_Controls().Add((Control)(object)label32);
			((Control)panel1).get_Controls().Add((Control)(object)cmdDuplicates);
			((Control)panel1).get_Controls().Add((Control)(object)cmdCheckSAO_XZ);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)lblToEditCount);
			((Control)panel1).get_Controls().Add((Control)(object)pBarErrors);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)lstErrors);
			((Control)panel1).get_Controls().Add((Control)(object)cmdCheckForErrors);
			((Control)panel1).get_Controls().Add((Control)(object)label35);
			((Control)panel1).get_Controls().Add((Control)(object)label36);
			((Control)panel1).set_Location(new Point(1, 37));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(162, 404));
			((Control)panel1).set_TabIndex(1);
			((Control)cmdCancelCheck).set_BackColor(Color.Pink);
			((Control)cmdCancelCheck).set_Location(new Point(23, 76));
			((Control)cmdCancelCheck).set_Name("cmdCancelCheck");
			((Control)cmdCancelCheck).set_Size(new Size(113, 23));
			((Control)cmdCancelCheck).set_TabIndex(19);
			((Control)cmdCancelCheck).set_Text("Cancel check");
			((ButtonBase)cmdCancelCheck).set_UseVisualStyleBackColor(false);
			((Control)cmdCancelCheck).set_Visible(false);
			((Control)cmdCancelCheck).add_Click((EventHandler)cmdCancelCheck_Click);
			((Control)chkEmbargoed).set_AutoSize(true);
			chkEmbargoed.set_Checked(true);
			chkEmbargoed.set_CheckState((CheckState)1);
			((Control)chkEmbargoed).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkEmbargoed).set_Location(new Point(24, 75));
			((Control)chkEmbargoed).set_Name("chkEmbargoed");
			((Control)chkEmbargoed).set_Size(new Size(119, 17));
			((Control)chkEmbargoed).set_TabIndex(18);
			((Control)chkEmbargoed).set_Text("Exclude embargoed");
			((ButtonBase)chkEmbargoed).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotAll).set_BackColor(Color.Khaki);
			((Control)cmdPlotAll).set_Location(new Point(17, 368));
			((Control)cmdPlotAll).set_Name("cmdPlotAll");
			((Control)cmdPlotAll).set_Size(new Size(127, 31));
			((Control)cmdPlotAll).set_TabIndex(17);
			((Control)cmdPlotAll).set_Text("Plot all listed curves");
			((ButtonBase)cmdPlotAll).set_UseVisualStyleBackColor(false);
			((Control)cmdPlotAll).add_Click((EventHandler)cmdPlotAll_Click);
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(3, 50));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(18, 13));
			((Control)label34).set_TabIndex(14);
			((Control)label34).set_Text("2.");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(3, 106));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(18, 13));
			((Control)label33).set_TabIndex(16);
			((Control)label33).set_Text("3.");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(3, 14));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(18, 13));
			((Control)label32).set_TabIndex(12);
			((Control)label32).set_Text("1.");
			((Control)cmdDuplicates).set_BackColor(Color.LightYellow);
			((Control)cmdDuplicates).set_Location(new Point(20, 100));
			((Control)cmdDuplicates).set_Name("cmdDuplicates");
			((Control)cmdDuplicates).set_Size(new Size(120, 26));
			((Control)cmdDuplicates).set_TabIndex(2);
			((Control)cmdDuplicates).set_Text("Duplicate light curves");
			toolTip1.SetToolTip((Control)(object)cmdDuplicates, "Checks to ensure all the star fields for lunar occultations are completed");
			((ButtonBase)cmdDuplicates).set_UseVisualStyleBackColor(false);
			((Control)cmdDuplicates).add_Click((EventHandler)cmdDuplicates_Click);
			((Control)cmdCheckSAO_XZ).set_BackColor(Color.LightCyan);
			((Control)cmdCheckSAO_XZ).set_Location(new Point(20, 8));
			((Control)cmdCheckSAO_XZ).set_Name("cmdCheckSAO_XZ");
			((Control)cmdCheckSAO_XZ).set_Size(new Size(120, 26));
			((Control)cmdCheckSAO_XZ).set_TabIndex(0);
			((Control)cmdCheckSAO_XZ).set_Text(" XZ/SAO consistency");
			toolTip1.SetToolTip((Control)(object)cmdCheckSAO_XZ, "Checks to ensure all the star fields for lunar occultations are completed");
			((ButtonBase)cmdCheckSAO_XZ).set_UseVisualStyleBackColor(false);
			((Control)cmdCheckSAO_XZ).add_Click((EventHandler)cmdCheckSAO_XZ_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(31, 147));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(98, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("Double-click to edit");
			label2.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lblToEditCount).set_AutoSize(true);
			((Control)lblToEditCount).set_Location(new Point(40, 351));
			((Control)lblToEditCount).set_Name("lblToEditCount");
			((Control)lblToEditCount).set_Size(new Size(80, 13));
			((Control)lblToEditCount).set_TabIndex(5);
			((Control)lblToEditCount).set_Text("0 curves to edit");
			((Control)pBarErrors).set_Location(new Point(21, 354));
			((Control)pBarErrors).set_Name("pBarErrors");
			((Control)pBarErrors).set_Size(new Size(118, 9));
			((Control)pBarErrors).set_TabIndex(6);
			((Control)pBarErrors).set_Visible(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(22, 131));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(116, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Light curves with errors");
			((ListControl)lstErrors).set_FormattingEnabled(true);
			((Control)lstErrors).set_Location(new Point(20, 163));
			((Control)lstErrors).set_Name("lstErrors");
			((Control)lstErrors).set_Size(new Size(120, 186));
			((Control)lstErrors).set_TabIndex(4);
			((Control)lstErrors).add_MouseDoubleClick(new MouseEventHandler(lstErrors_MouseDoubleClick));
			((Control)cmdCheckForErrors).set_BackColor(Color.Honeydew);
			((Control)cmdCheckForErrors).set_Location(new Point(20, 38));
			((Control)cmdCheckForErrors).set_Name("cmdCheckForErrors");
			((Control)cmdCheckForErrors).set_Size(new Size(120, 36));
			((Control)cmdCheckForErrors).set_TabIndex(1);
			((Control)cmdCheckForErrors).set_Text("Check light curves \r\nfor general errors");
			((ButtonBase)cmdCheckForErrors).set_UseVisualStyleBackColor(false);
			((Control)cmdCheckForErrors).add_Click((EventHandler)cmdCheckForErrors_Click);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 9.4f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(4, 28));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(14, 16));
			((Control)label35).set_TabIndex(13);
			((Control)label35).set_Text("↓");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 9.4f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(3, 78));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(14, 16));
			((Control)label36).set_TabIndex(15);
			((Control)label36).set_Text("↓");
			((Control)lblMainUpdated).set_AutoSize(true);
			((Control)lblMainUpdated).set_BackColor(Color.LavenderBlush);
			((Control)lblMainUpdated).set_Font(new Font("Microsoft Sans Serif", 9.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMainUpdated).set_ForeColor(Color.Indigo);
			((Control)lblMainUpdated).set_Location(new Point(331, 479));
			((Control)lblMainUpdated).set_Name("lblMainUpdated");
			((Control)lblMainUpdated).set_Size(new Size(147, 16));
			((Control)lblMainUpdated).set_TabIndex(10);
			((Control)lblMainUpdated).set_Text("Light curves updated:  0");
			((Control)cmdEdit).set_Location(new Point(97, 27));
			((Control)cmdEdit).set_Name("cmdEdit");
			((Control)cmdEdit).set_Size(new Size(38, 25));
			((Control)cmdEdit).set_TabIndex(9);
			((Control)cmdEdit).set_Text("Edit");
			((ButtonBase)cmdEdit).set_UseVisualStyleBackColor(true);
			((Control)cmdEdit).add_Click((EventHandler)cmdEdit_Click);
			((Control)cmdZerovalues).set_BackColor(Color.DarkBlue);
			((Control)cmdZerovalues).set_ForeColor(Color.Yellow);
			((Control)cmdZerovalues).set_Location(new Point(11, 26));
			((Control)cmdZerovalues).set_Name("cmdZerovalues");
			((Control)cmdZerovalues).set_Size(new Size(132, 31));
			((Control)cmdZerovalues).set_TabIndex(18);
			((Control)cmdZerovalues).set_Text("List possible light curves ");
			((ButtonBase)cmdZerovalues).set_UseVisualStyleBackColor(false);
			((Control)cmdZerovalues).set_Visible(false);
			((Control)cmdZerovalues).add_Click((EventHandler)cmdZerovalues_Click);
			((Control)updnYear).set_Location(new Point(16, 30));
			updnYear.set_Maximum(new decimal(new int[4] { 2040, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(51, 20));
			((Control)updnYear).set_TabIndex(1);
			updnYear.set_Value(new decimal(new int[4] { 2022, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblDate);
			((Control)groupBox1).get_Controls().Add((Control)(object)label12);
			((Control)groupBox1).get_Controls().Add((Control)(object)label13);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).get_Controls().Add((Control)(object)label16);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnMonth);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnSecond);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnHour);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnMinute);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnDay);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnYear);
			((Control)groupBox1).set_Location(new Point(174, 68));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(337, 67));
			((Control)groupBox1).set_TabIndex(2);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Date && time for first point in light curve");
			((Control)lblDate).set_AutoSize(true);
			((Control)lblDate).set_ForeColor(Color.Brown);
			((Control)lblDate).set_Location(new Point(34, 51));
			((Control)lblDate).set_Name("lblDate");
			((Control)lblDate).set_Size(new Size(30, 13));
			((Control)lblDate).set_TabIndex(12);
			((Control)lblDate).set_Text("Date");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(272, 16));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(44, 13));
			((Control)label12).set_TabIndex(10);
			((Control)label12).set_Text("Second");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(231, 16));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(39, 13));
			((Control)label13).set_TabIndex(8);
			((Control)label13).set_Text("Minute");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(184, 16));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(30, 13));
			((Control)label14).set_TabIndex(6);
			((Control)label14).set_Text("Hour");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(120, 16));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(26, 13));
			((Control)label15).set_TabIndex(4);
			((Control)label15).set_Text("Day");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(70, 16));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(37, 13));
			((Control)label16).set_TabIndex(2);
			((Control)label16).set_Text("Month");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(13, 16));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(29, 13));
			((Control)label17).set_TabIndex(0);
			((Control)label17).set_Text("Year");
			((Control)updnMonth).set_Location(new Point(73, 30));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(40, 20));
			((Control)updnMonth).set_TabIndex(3);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			updnSecond.set_DecimalPlaces(2);
			((Control)updnSecond).set_Location(new Point(276, 30));
			updnSecond.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)updnSecond).set_Name("updnSecond");
			((Control)updnSecond).set_Size(new Size(51, 20));
			((Control)updnSecond).set_TabIndex(11);
			((Control)updnSecond).add_Enter((EventHandler)updnSecond_Enter);
			((Control)updnHour).set_Location(new Point(188, 30));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(35, 20));
			((Control)updnHour).set_TabIndex(7);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			((Control)updnMinute).set_Location(new Point(231, 30));
			updnMinute.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(36, 20));
			((Control)updnMinute).set_TabIndex(9);
			((Control)updnMinute).add_Enter((EventHandler)updnMinute_Enter);
			((Control)updnDay).set_Location(new Point(121, 30));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(40, 20));
			((Control)updnDay).set_TabIndex(5);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)panelSetInvalid).set_BackColor(Color.OldLace);
			panelSetInvalid.set_BorderStyle((BorderStyle)1);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label70);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)panel8);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)panel5);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label55);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label54);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label53);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label52);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label51);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label50);
			((Control)panelSetInvalid).get_Controls().Add((Control)(object)label38);
			((Control)panelSetInvalid).set_Location(new Point(472, 63));
			((Control)panelSetInvalid).set_Name("panelSetInvalid");
			((Control)panelSetInvalid).set_Size(new Size(264, 327));
			((Control)panelSetInvalid).set_TabIndex(36);
			((Control)panelSetInvalid).set_Visible(false);
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label70).set_Location(new Point(66, 246));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(130, 13));
			((Control)label70).set_TabIndex(16);
			((Control)label70).set_Text("Set a frame as invalid");
			((Control)panel8).set_BackColor(Color.Khaki);
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)cmdSetFrameValid);
			((Control)panel8).get_Controls().Add((Control)(object)cmdSetFrameInvalid);
			((Control)panel8).get_Controls().Add((Control)(object)txtSingle);
			((Control)panel8).get_Controls().Add((Control)(object)optSingle);
			((Control)panel8).set_Location(new Point(9, 262));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(244, 56));
			((Control)panel8).set_TabIndex(15);
			((Control)cmdSetFrameValid).set_BackColor(Color.GreenYellow);
			((Control)cmdSetFrameValid).set_Location(new Point(125, 27));
			((Control)cmdSetFrameValid).set_Name("cmdSetFrameValid");
			((Control)cmdSetFrameValid).set_Size(new Size(83, 22));
			((Control)cmdSetFrameValid).set_TabIndex(11);
			((Control)cmdSetFrameValid).set_Text("Set to Valid");
			((ButtonBase)cmdSetFrameValid).set_UseVisualStyleBackColor(false);
			((Control)cmdSetFrameValid).add_Click((EventHandler)cmdSetFrameValid_Click);
			((Control)cmdSetFrameInvalid).set_BackColor(Color.LightSalmon);
			((Control)cmdSetFrameInvalid).set_Location(new Point(125, 3));
			((Control)cmdSetFrameInvalid).set_Name("cmdSetFrameInvalid");
			((Control)cmdSetFrameInvalid).set_Size(new Size(83, 22));
			((Control)cmdSetFrameInvalid).set_TabIndex(10);
			((Control)cmdSetFrameInvalid).set_Text("Set to Invalid");
			((ButtonBase)cmdSetFrameInvalid).set_UseVisualStyleBackColor(false);
			((Control)cmdSetFrameInvalid).add_Click((EventHandler)cmdSetFrameInvalid_Click);
			((Control)txtSingle).set_Location(new Point(61, 16));
			((Control)txtSingle).set_Name("txtSingle");
			((Control)txtSingle).set_Size(new Size(37, 20));
			((Control)txtSingle).set_TabIndex(9);
			optSingle.set_AutoCheck(false);
			((Control)optSingle).set_AutoSize(true);
			((Control)optSingle).set_Location(new Point(7, 18));
			((Control)optSingle).set_Name("optSingle");
			((Control)optSingle).set_Size(new Size(54, 17));
			((Control)optSingle).set_TabIndex(8);
			optSingle.set_TabStop(true);
			((Control)optSingle).set_Text("Frame");
			((ButtonBase)optSingle).set_UseVisualStyleBackColor(true);
			((Control)optSingle).add_Click((EventHandler)optSingle_Click);
			((Control)panel5).set_BackColor(Color.LightCyan);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)cmdSelectAll);
			((Control)panel5).get_Controls().Add((Control)(object)txtEnd);
			((Control)panel5).get_Controls().Add((Control)(object)txtStart);
			((Control)panel5).get_Controls().Add((Control)(object)optEnd);
			((Control)panel5).get_Controls().Add((Control)(object)optStart);
			((Control)panel5).get_Controls().Add((Control)(object)cmdSetInvalid);
			((Control)panel5).get_Controls().Add((Control)(object)cmdResetToValid);
			((Control)panel5).set_Location(new Point(0, 122));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(253, 111));
			((Control)panel5).set_TabIndex(14);
			((Control)cmdSelectAll).set_BackColor(Color.LightGreen);
			((Control)cmdSelectAll).set_Location(new Point(16, 65));
			((Control)cmdSelectAll).set_Name("cmdSelectAll");
			((Control)cmdSelectAll).set_Size(new Size(92, 36));
			((Control)cmdSelectAll).set_TabIndex(7);
			((Control)cmdSelectAll).set_Text("Select the entire\r\nlight curve");
			((ButtonBase)cmdSelectAll).set_UseVisualStyleBackColor(false);
			((Control)cmdSelectAll).add_Click((EventHandler)cmdSelectAll_Click);
			((Control)txtEnd).set_Location(new Point(78, 38));
			((Control)txtEnd).set_Name("txtEnd");
			((Control)txtEnd).set_Size(new Size(37, 20));
			((Control)txtEnd).set_TabIndex(4);
			((Control)txtStart).set_Location(new Point(78, 10));
			((Control)txtStart).set_Name("txtStart");
			((Control)txtStart).set_Size(new Size(37, 20));
			((Control)txtStart).set_TabIndex(3);
			optEnd.set_AutoCheck(false);
			((Control)optEnd).set_AutoSize(true);
			((Control)optEnd).set_Location(new Point(3, 40));
			((Control)optEnd).set_Name("optEnd");
			((Control)optEnd).set_Size(new Size(70, 17));
			((Control)optEnd).set_TabIndex(1);
			((Control)optEnd).set_Text("End point");
			((ButtonBase)optEnd).set_UseVisualStyleBackColor(true);
			((Control)optEnd).add_Click((EventHandler)optEnd_Click);
			optStart.set_AutoCheck(false);
			((Control)optStart).set_AutoSize(true);
			optStart.set_Checked(true);
			((Control)optStart).set_Location(new Point(3, 12));
			((Control)optStart).set_Name("optStart");
			((Control)optStart).set_Size(new Size(73, 17));
			((Control)optStart).set_TabIndex(0);
			optStart.set_TabStop(true);
			((Control)optStart).set_Text("Start point");
			((ButtonBase)optStart).set_UseVisualStyleBackColor(true);
			((Control)optStart).add_Click((EventHandler)optStart_Click);
			((Control)cmdSetInvalid).set_BackColor(Color.Aqua);
			((Control)cmdSetInvalid).set_Location(new Point(120, 4));
			((Control)cmdSetInvalid).set_Name("cmdSetInvalid");
			((Control)cmdSetInvalid).set_Size(new Size(127, 48));
			((Control)cmdSetInvalid).set_TabIndex(2);
			((Control)cmdSetInvalid).set_Text("Set frames in selected\r\nregion as skipped frames");
			((ButtonBase)cmdSetInvalid).set_UseVisualStyleBackColor(false);
			((Control)cmdSetInvalid).add_Click((EventHandler)cmdSetInvalid_Click);
			((Control)cmdResetToValid).set_BackColor(Color.GreenYellow);
			((Control)cmdResetToValid).set_Location(new Point(122, 63));
			((Control)cmdResetToValid).set_Name("cmdResetToValid");
			((Control)cmdResetToValid).set_Size(new Size(122, 42));
			((Control)cmdResetToValid).set_TabIndex(6);
			((Control)cmdResetToValid).set_Text("Set frames in selected\r\nregion as valid frames");
			((ButtonBase)cmdResetToValid).set_UseVisualStyleBackColor(false);
			((Control)cmdResetToValid).add_Click((EventHandler)cmdResetToValid_Click);
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Location(new Point(3, 75));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(241, 13));
			((Control)label55).set_TabIndex(13);
			((Control)label55).set_Text("*  Look for drops inconsistent with adjacent points");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Location(new Point(3, 47));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(255, 13));
			((Control)label54).set_TabIndex(12);
			((Control)label54).set_Text("*  Dashed red lines indicate 'possible' skipped frames");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Location(new Point(3, 61));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(231, 13));
			((Control)label53).set_TabIndex(11);
			((Control)label53).set_Text("*  Solid red lines indicate frames set as 'skipped'");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Location(new Point(3, 33));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(219, 13));
			((Control)label52).set_TabIndex(10);
			((Control)label52).set_Text("*  Only some 'zero' frames are skipped frames");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label51).set_ForeColor(Color.Red);
			((Control)label51).set_Location(new Point(3, 19));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(250, 13));
			((Control)label51).set_TabIndex(9);
			((Control)label51).set_Text("*  Skipped frames will have a value of zero");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(1, 2));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(254, 13));
			((Control)label50).set_TabIndex(8);
			((Control)label50).set_Text("Skipped frames.   Detect, and set to invalid");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_ForeColor(Color.DarkRed);
			((Control)label38).set_Location(new Point(3, 94));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(240, 26));
			((Control)label38).set_TabIndex(5);
			((Control)label38).set_Text("*  To set the region to be processed, Click plot at \r\n      the start and end of the region");
			((Control)lblDurn).set_AutoSize(true);
			((Control)lblDurn).set_Location(new Point(12, 16));
			((Control)lblDurn).set_Name("lblDurn");
			((Control)lblDurn).set_Size(new Size(78, 13));
			((Control)lblDurn).set_TabIndex(0);
			((Control)lblDurn).set_Text("Duration (secs)");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(5, 17));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(44, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Number");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(253, 15));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(42, 13));
			((Control)label5).set_TabIndex(16);
			((Control)label5).set_Text("UCAC4");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(124, 15));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(43, 13));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("Tycho2");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(484, 15));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(21, 13));
			((Control)label7).set_TabIndex(20);
			((Control)label7).set_Text("ZC");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(346, 15));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(29, 13));
			((Control)label8).set_TabIndex(18);
			((Control)label8).set_Text("SAO");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(2, 15));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(55, 13));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text("Hipparcos");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(12, 19));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(35, 13));
			((Control)label10).set_TabIndex(0);
			((Control)label10).set_Text("Name");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(115, 16));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(45, 13));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("# points");
			((Control)groupBox2).get_Controls().Add((Control)(object)lblLight);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnPoints);
			((Control)groupBox2).get_Controls().Add((Control)(object)label11);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnDuration);
			((Control)groupBox2).get_Controls().Add((Control)(object)lblDurn);
			((Control)groupBox2).set_Location(new Point(531, 68));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(187, 67));
			((Control)groupBox2).set_TabIndex(3);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Light curve parameters");
			((Control)lblLight).set_AutoSize(true);
			((Control)lblLight).set_ForeColor(Color.Brown);
			((Control)lblLight).set_Location(new Point(17, 51));
			((Control)lblLight).set_Name("lblLight");
			((Control)lblLight).set_Size(new Size(60, 13));
			((Control)lblLight).set_TabIndex(4);
			((Control)lblLight).set_Text("Light curve");
			((Control)updnPoints).set_Location(new Point(115, 30));
			updnPoints.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			((Control)updnPoints).set_Name("updnPoints");
			((Control)updnPoints).set_Size(new Size(52, 20));
			((Control)updnPoints).set_TabIndex(3);
			((Control)updnPoints).add_Enter((EventHandler)updnPoints_Enter);
			updnDuration.set_DecimalPlaces(2);
			((Control)updnDuration).set_Location(new Point(15, 30));
			updnDuration.set_Maximum(new decimal(new int[4] { 7200, 0, 0, 0 }));
			((Control)updnDuration).set_Name("updnDuration");
			((Control)updnDuration).set_Size(new Size(61, 20));
			((Control)updnDuration).set_TabIndex(1);
			((Control)updnDuration).add_Enter((EventHandler)updnDuration_Enter);
			((Control)groupBox3).get_Controls().Add((Control)(object)lblObserver);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtNS);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtEW);
			((Control)groupBox3).get_Controls().Add((Control)(object)label25);
			((Control)groupBox3).get_Controls().Add((Control)(object)label24);
			((Control)groupBox3).get_Controls().Add((Control)(object)label10);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnAlt);
			((Control)groupBox3).get_Controls().Add((Control)(object)label23);
			((Control)groupBox3).get_Controls().Add((Control)(object)label20);
			((Control)groupBox3).get_Controls().Add((Control)(object)label21);
			((Control)groupBox3).get_Controls().Add((Control)(object)label22);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLatSec);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLatDeg);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLatMin);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label18);
			((Control)groupBox3).get_Controls().Add((Control)(object)label19);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLongSec);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLongDeg);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLongMin);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtObserver);
			((Control)groupBox3).set_Location(new Point(174, 141));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(605, 72));
			((Control)groupBox3).set_TabIndex(4);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Observer");
			((Control)lblObserver).set_AutoSize(true);
			((Control)lblObserver).set_ForeColor(Color.Brown);
			((Control)lblObserver).set_Location(new Point(34, 56));
			((Control)lblObserver).set_Name("lblObserver");
			((Control)lblObserver).set_Size(new Size(50, 13));
			((Control)lblObserver).set_TabIndex(18);
			((Control)lblObserver).set_Text("Observer");
			((Control)txtNS).set_Location(new Point(366, 34));
			((Control)txtNS).set_Name("txtNS");
			((Control)txtNS).set_Size(new Size(11, 20));
			((Control)txtNS).set_TabIndex(10);
			((Control)txtNS).set_Text("+");
			((Control)txtEW).set_Location(new Point(188, 34));
			((Control)txtEW).set_Name("txtEW");
			((Control)txtEW).set_Size(new Size(12, 20));
			((Control)txtEW).set_TabIndex(2);
			((Control)txtEW).set_Text("+");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(373, 8));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(45, 13));
			((Control)label25).set_TabIndex(13);
			((Control)label25).set_Text("Latitude");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(199, 8));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(54, 13));
			((Control)label24).set_TabIndex(5);
			((Control)label24).set_Text("Longitude");
			((Control)updnAlt).set_Location(new Point(539, 35));
			updnAlt.set_Maximum(new decimal(new int[4] { 5000, 0, 0, 0 }));
			updnAlt.set_Minimum(new decimal(new int[4] { 200, 0, 0, -2147483648 }));
			((Control)updnAlt).set_Name("updnAlt");
			((Control)updnAlt).set_Size(new Size(52, 20));
			((Control)updnAlt).set_TabIndex(18);
			((Control)updnAlt).add_Enter((EventHandler)updnAlt_Enter);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(539, 21));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(59, 13));
			((Control)label23).set_TabIndex(19);
			((Control)label23).set_Text("Altitude (m)");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(465, 21));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(44, 13));
			((Control)label20).set_TabIndex(17);
			((Control)label20).set_Text("Second");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(424, 21));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(39, 13));
			((Control)label21).set_TabIndex(15);
			((Control)label21).set_Text("Minute");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(373, 21));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(42, 13));
			((Control)label22).set_TabIndex(12);
			((Control)label22).set_Text("Degree");
			updnLatSec.set_DecimalPlaces(1);
			((Control)updnLatSec).set_Location(new Point(469, 35));
			updnLatSec.set_Maximum(new decimal(new int[4] { 599, 0, 0, 65536 }));
			((Control)updnLatSec).set_Name("updnLatSec");
			((Control)updnLatSec).set_Size(new Size(51, 20));
			((Control)updnLatSec).set_TabIndex(16);
			((Control)updnLatSec).add_Enter((EventHandler)updnLatSec_Enter);
			((Control)updnLatDeg).set_Location(new Point(377, 35));
			updnLatDeg.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			updnLatDeg.set_Minimum(new decimal(new int[4] { 89, 0, 0, -2147483648 }));
			((Control)updnLatDeg).set_Name("updnLatDeg");
			((Control)updnLatDeg).set_Size(new Size(40, 20));
			((Control)updnLatDeg).set_TabIndex(11);
			((Control)updnLatDeg).add_Enter((EventHandler)updnLatDeg_Enter);
			((Control)updnLatMin).set_Location(new Point(424, 35));
			updnLatMin.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnLatMin).set_Name("updnLatMin");
			((Control)updnLatMin).set_Size(new Size(36, 20));
			((Control)updnLatMin).set_TabIndex(14);
			((Control)updnLatMin).add_Enter((EventHandler)updnLatMin_Enter);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(298, 21));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(44, 13));
			((Control)label3).set_TabIndex(9);
			((Control)label3).set_Text("Second");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(257, 21));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(39, 13));
			((Control)label18).set_TabIndex(7);
			((Control)label18).set_Text("Minute");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(200, 21));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(42, 13));
			((Control)label19).set_TabIndex(4);
			((Control)label19).set_Text("Degree");
			updnLongSec.set_DecimalPlaces(1);
			((Control)updnLongSec).set_Location(new Point(302, 35));
			updnLongSec.set_Maximum(new decimal(new int[4] { 599, 0, 0, 65536 }));
			((Control)updnLongSec).set_Name("updnLongSec");
			((Control)updnLongSec).set_Size(new Size(51, 20));
			((Control)updnLongSec).set_TabIndex(8);
			((Control)updnLongSec).add_Enter((EventHandler)updnLongSec_Enter);
			((Control)updnLongDeg).set_Location(new Point(200, 35));
			updnLongDeg.set_Maximum(new decimal(new int[4] { 359, 0, 0, 0 }));
			updnLongDeg.set_Minimum(new decimal(new int[4] { 179, 0, 0, -2147483648 }));
			((Control)updnLongDeg).set_Name("updnLongDeg");
			((Control)updnLongDeg).set_Size(new Size(50, 20));
			((Control)updnLongDeg).set_TabIndex(3);
			((Control)updnLongDeg).add_Enter((EventHandler)updnLongDeg_Enter);
			((Control)updnLongMin).set_Location(new Point(257, 35));
			updnLongMin.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnLongMin).set_Name("updnLongMin");
			((Control)updnLongMin).set_Size(new Size(36, 20));
			((Control)updnLongMin).set_TabIndex(6);
			((Control)updnLongMin).add_Enter((EventHandler)updnLongMin_Enter);
			((Control)txtObserver).set_Location(new Point(10, 34));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(165, 20));
			((Control)txtObserver).set_TabIndex(1);
			((Control)txtObserver).add_Enter((EventHandler)txtObserver_Enter);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdUpdateSAO);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdUpdateXZ);
			((Control)groupBox4).get_Controls().Add((Control)(object)lblStarLine);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnTb);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnU4b);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnTa);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnTc);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnU4a);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnSAO);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnXZ);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnZC);
			((Control)groupBox4).get_Controls().Add((Control)(object)updnHip);
			((Control)groupBox4).get_Controls().Add((Control)(object)label26);
			((Control)groupBox4).get_Controls().Add((Control)(object)label6);
			((Control)groupBox4).get_Controls().Add((Control)(object)label7);
			((Control)groupBox4).get_Controls().Add((Control)(object)label8);
			((Control)groupBox4).get_Controls().Add((Control)(object)label9);
			((Control)groupBox4).get_Controls().Add((Control)(object)label5);
			((Control)groupBox4).get_Controls().Add((Control)(object)label31);
			((Control)groupBox4).get_Controls().Add((Control)(object)label30);
			((Control)groupBox4).get_Controls().Add((Control)(object)label27);
			((Control)groupBox4).set_Location(new Point(174, 219));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(605, 67));
			((Control)groupBox4).set_TabIndex(5);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Star");
			((Control)cmdUpdateSAO).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUpdateSAO).set_Location(new Point(530, 37));
			((Control)cmdUpdateSAO).set_Name("cmdUpdateSAO");
			((Control)cmdUpdateSAO).set_Size(new Size(66, 20));
			((Control)cmdUpdateSAO).set_TabIndex(11);
			((Control)cmdUpdateSAO).set_Text("Update SAO");
			((ButtonBase)cmdUpdateSAO).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateSAO).add_Click((EventHandler)cmdUpdateSAO_Click);
			((Control)cmdUpdateXZ).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUpdateXZ).set_Location(new Point(530, 17));
			((Control)cmdUpdateXZ).set_Name("cmdUpdateXZ");
			((Control)cmdUpdateXZ).set_Size(new Size(66, 20));
			((Control)cmdUpdateXZ).set_TabIndex(10);
			((Control)cmdUpdateXZ).set_Text("Update XZ");
			((ButtonBase)cmdUpdateXZ).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateXZ).add_Click((EventHandler)cmdUpdateXZ_Click);
			((Control)lblStarLine).set_AutoSize(true);
			((Control)lblStarLine).set_ForeColor(Color.Brown);
			((Control)lblStarLine).set_Location(new Point(34, 52));
			((Control)lblStarLine).set_Name("lblStarLine");
			((Control)lblStarLine).set_Size(new Size(26, 13));
			((Control)lblStarLine).set_TabIndex(12);
			((Control)lblStarLine).set_Text("Star");
			((Control)updnTb).set_Location(new Point(126, 29));
			updnTb.set_Maximum(new decimal(new int[4] { 99999, 0, 0, 0 }));
			((Control)updnTb).set_Name("updnTb");
			((Control)updnTb).set_Size(new Size(51, 20));
			((Control)updnTb).set_TabIndex(3);
			((Control)updnU4b).set_Location(new Point(273, 30));
			updnU4b.set_Maximum(new decimal(new int[4] { 290000, 0, 0, 0 }));
			((Control)updnU4b).set_Name("updnU4b");
			((Control)updnU4b).set_Size(new Size(59, 20));
			((Control)updnU4b).set_TabIndex(6);
			((Control)updnTa).set_Location(new Point(72, 29));
			updnTa.set_Maximum(new decimal(new int[4] { 8000, 0, 0, 0 }));
			((Control)updnTa).set_Name("updnTa");
			((Control)updnTa).set_Size(new Size(48, 20));
			((Control)updnTa).set_TabIndex(2);
			((Control)updnTc).set_Location(new Point(183, 30));
			updnTc.set_Maximum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnTc).set_Name("updnTc");
			((Control)updnTc).set_Size(new Size(31, 20));
			((Control)updnTc).set_TabIndex(4);
			updnTc.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnU4a).set_Location(new Point(224, 30));
			updnU4a.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			((Control)updnU4a).set_Name("updnU4a");
			((Control)updnU4a).set_Size(new Size(43, 20));
			((Control)updnU4a).set_TabIndex(5);
			((Control)updnSAO).set_Location(new Point(342, 30));
			updnSAO.set_Maximum(new decimal(new int[4] { 258997, 0, 0, 0 }));
			((Control)updnSAO).set_Name("updnSAO");
			((Control)updnSAO).set_Size(new Size(59, 20));
			((Control)updnSAO).set_TabIndex(7);
			updnSAO.add_ValueChanged((EventHandler)updnSAO_ValueChanged);
			((Control)updnXZ).set_Location(new Point(410, 30));
			updnXZ.set_Maximum(new decimal(new int[4] { 244440, 0, 0, 0 }));
			((Control)updnXZ).set_Name("updnXZ");
			((Control)updnXZ).set_Size(new Size(59, 20));
			((Control)updnXZ).set_TabIndex(8);
			((Control)updnZC).set_Enabled(false);
			((Control)updnZC).set_Location(new Point(475, 30));
			updnZC.set_Maximum(new decimal(new int[4] { 5000, 0, 0, 0 }));
			((Control)updnZC).set_Name("updnZC");
			((Control)updnZC).set_Size(new Size(46, 20));
			((Control)updnZC).set_TabIndex(9);
			updnZC.add_ValueChanged((EventHandler)updnZC_ValueChanged);
			((Control)updnHip).set_Location(new Point(4, 30));
			updnHip.set_Maximum(new decimal(new int[4] { 120404, 0, 0, 0 }));
			((Control)updnHip).set_Name("updnHip");
			((Control)updnHip).set_Size(new Size(59, 20));
			((Control)updnHip).set_TabIndex(1);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(420, 15));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(21, 13));
			((Control)label26).set_TabIndex(19);
			((Control)label26).set_Text("XZ");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(119, 33));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(10, 13));
			((Control)label31).set_TabIndex(13);
			((Control)label31).set_Text("-");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Location(new Point(175, 34));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(10, 13));
			((Control)label30).set_TabIndex(15);
			((Control)label30).set_Text("-");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(265, 34));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(10, 13));
			((Control)label27).set_TabIndex(17);
			((Control)label27).set_Text("-");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Location(new Point(0, 36));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(35, 13));
			((Control)label39).set_TabIndex(2);
			((Control)label39).set_Text("Name");
			((Control)groupAsteroid).get_Controls().Add((Control)(object)cmdGetName);
			((Control)groupAsteroid).get_Controls().Add((Control)(object)updnAsteroidNo);
			((Control)groupAsteroid).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)groupAsteroid).get_Controls().Add((Control)(object)label39);
			((Control)groupAsteroid).get_Controls().Add((Control)(object)lblAsteroid);
			((Control)groupAsteroid).get_Controls().Add((Control)(object)label4);
			((Control)groupAsteroid).set_Location(new Point(287, 292));
			((Control)groupAsteroid).set_Name("groupAsteroid");
			((Control)groupAsteroid).set_Size(new Size(191, 77));
			((Control)groupAsteroid).set_TabIndex(7);
			groupAsteroid.set_TabStop(false);
			((Control)groupAsteroid).set_Text("Asteroid details");
			((Control)cmdGetName).set_Location(new Point(121, 12));
			((Control)cmdGetName).set_Name("cmdGetName");
			((Control)cmdGetName).set_Size(new Size(65, 22));
			((Control)cmdGetName).set_TabIndex(4);
			((Control)cmdGetName).set_Text("Get name");
			((ButtonBase)cmdGetName).set_UseVisualStyleBackColor(true);
			((Control)cmdGetName).add_Click((EventHandler)cmdGetName_Click);
			((Control)updnAsteroidNo).set_Location(new Point(58, 13));
			updnAsteroidNo.set_Maximum(new decimal(new int[4] { 999999, 0, 0, 0 }));
			((Control)updnAsteroidNo).set_Name("updnAsteroidNo");
			((Control)updnAsteroidNo).set_Size(new Size(59, 20));
			((Control)updnAsteroidNo).set_TabIndex(1);
			((Control)updnAsteroidNo).add_Enter((EventHandler)updnAsteroidNo_Enter);
			((Control)txtAsteroidName).set_Location(new Point(35, 35));
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((Control)txtAsteroidName).set_Size(new Size(119, 20));
			((Control)txtAsteroidName).set_TabIndex(3);
			((Control)lblAsteroid).set_AutoSize(true);
			((Control)lblAsteroid).set_ForeColor(Color.Brown);
			((Control)lblAsteroid).set_Location(new Point(6, 58));
			((Control)lblAsteroid).set_Name("lblAsteroid");
			((Control)lblAsteroid).set_Size(new Size(45, 13));
			((Control)lblAsteroid).set_TabIndex(5);
			((Control)lblAsteroid).set_Text("Asteroid");
			((Control)grpMoon).get_Controls().Add((Control)(object)cmbNS);
			((Control)grpMoon).get_Controls().Add((Control)(object)label48);
			((Control)grpMoon).get_Controls().Add((Control)(object)label47);
			((Control)grpMoon).get_Controls().Add((Control)(object)label46);
			((Control)grpMoon).get_Controls().Add((Control)(object)label45);
			((Control)grpMoon).get_Controls().Add((Control)(object)label44);
			((Control)grpMoon).get_Controls().Add((Control)(object)label43);
			((Control)grpMoon).get_Controls().Add((Control)(object)label42);
			((Control)grpMoon).get_Controls().Add((Control)(object)label41);
			((Control)grpMoon).get_Controls().Add((Control)(object)label40);
			((Control)grpMoon).get_Controls().Add((Control)(object)label29);
			((Control)grpMoon).get_Controls().Add((Control)(object)label28);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnLibL);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnLibB);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnSlope);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnContact);
			((Control)grpMoon).get_Controls().Add((Control)(object)lblMoon);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnRadius);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnMoonAlt);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnIllum);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnCA);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnPA);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnLimbMotion);
			((Control)grpMoon).get_Controls().Add((Control)(object)updnAA);
			((Control)grpMoon).set_Enabled(false);
			((Control)grpMoon).set_Location(new Point(515, 292));
			((Control)grpMoon).set_Name("grpMoon");
			((Control)grpMoon).set_Size(new Size(320, 176));
			((Control)grpMoon).set_TabIndex(8);
			grpMoon.set_TabStop(false);
			((Control)grpMoon).set_Text("Moon details");
			((ListControl)cmbNS).set_FormattingEnabled(true);
			cmbNS.get_Items().AddRange(new object[4] { "N", "E", "S", "W" });
			((Control)cmbNS).set_Location(new Point(47, 105));
			((Control)cmbNS).set_Name("cmbNS");
			((Control)cmbNS).set_Size(new Size(31, 21));
			((Control)cmbNS).set_TabIndex(18);
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Location(new Point(96, 16));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(52, 13));
			((Control)label48).set_TabIndex(2);
			((Control)label48).set_Text("libration L");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Location(new Point(178, 16));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(53, 13));
			((Control)label47).set_TabIndex(4);
			((Control)label47).set_Text("libration B");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Location(new Point(257, 16));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(57, 13));
			((Control)label46).set_TabIndex(6);
			((Control)label46).set_Text("Limb slope");
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Location(new Point(7, 55));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(63, 13));
			((Control)label45).set_TabIndex(8);
			((Control)label45).set_Text("Limb motion");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(81, 56));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(74, 13));
			((Control)label44).set_TabIndex(10);
			((Control)label44).set_Text("Contact Angle");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(177, 56));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(44, 13));
			((Control)label43).set_TabIndex(12);
			((Control)label43).set_Text("☾ radius");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(9, 92));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(61, 13));
			((Control)label42).set_TabIndex(16);
			((Control)label42).set_Text("Cusp Angle");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Location(new Point(185, 93));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(42, 13));
			((Control)label41).set_TabIndex(21);
			((Control)label41).set_Text("Altitude");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(94, 93));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(69, 13));
			((Control)label40).set_TabIndex(19);
			((Control)label40).set_Text("% illumination");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(243, 56));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(74, 13));
			((Control)label29).set_TabIndex(14);
			((Control)label29).set_Text("Position Angle");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(11, 16));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(56, 13));
			((Control)label28).set_TabIndex(0);
			((Control)label28).set_Text("Axis Angle");
			updnLibL.set_DecimalPlaces(3);
			((Control)updnLibL).set_Location(new Point(96, 30));
			updnLibL.set_Maximum(new decimal(new int[4] { 95, 0, 0, 65536 }));
			updnLibL.set_Minimum(new decimal(new int[4] { 95, 0, 0, -2147418112 }));
			((Control)updnLibL).set_Name("updnLibL");
			((Control)updnLibL).set_Size(new Size(52, 20));
			((Control)updnLibL).set_TabIndex(3);
			((Control)updnLibL).add_Enter((EventHandler)updnLibL_Enter);
			updnLibB.set_DecimalPlaces(3);
			((Control)updnLibB).set_Location(new Point(178, 30));
			updnLibB.set_Maximum(new decimal(new int[4] { 9, 0, 0, 0 }));
			updnLibB.set_Minimum(new decimal(new int[4] { 9, 0, 0, -2147483648 }));
			((Control)updnLibB).set_Name("updnLibB");
			((Control)updnLibB).set_Size(new Size(52, 20));
			((Control)updnLibB).set_TabIndex(5);
			((Control)updnLibB).add_Enter((EventHandler)updnLibB_Enter);
			updnSlope.set_DecimalPlaces(2);
			((Control)updnSlope).set_Location(new Point(260, 30));
			updnSlope.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSlope.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSlope).set_Name("updnSlope");
			((Control)updnSlope).set_Size(new Size(51, 20));
			((Control)updnSlope).set_TabIndex(7);
			((Control)updnSlope).add_Enter((EventHandler)updnSlope_Enter);
			updnContact.set_DecimalPlaces(2);
			((Control)updnContact).set_Location(new Point(88, 70));
			updnContact.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnContact.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnContact).set_Name("updnContact");
			((Control)updnContact).set_Size(new Size(61, 20));
			((Control)updnContact).set_TabIndex(11);
			((Control)updnContact).add_Enter((EventHandler)updnContact_Enter);
			((Control)lblMoon).set_ForeColor(Color.Brown);
			((Control)lblMoon).set_Location(new Point(11, 135));
			((Control)lblMoon).set_Name("lblMoon");
			((Control)lblMoon).set_Size(new Size(300, 36));
			((Control)lblMoon).set_TabIndex(13);
			((Control)lblMoon).set_Text("Moon");
			updnRadius.set_DecimalPlaces(3);
			updnRadius.set_Increment(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnRadius).set_Location(new Point(174, 70));
			updnRadius.set_Maximum(new decimal(new int[4] { 12, 0, 0, 65536 }));
			updnRadius.set_Minimum(new decimal(new int[4] { 8, 0, 0, 65536 }));
			((Control)updnRadius).set_Name("updnRadius");
			((Control)updnRadius).set_Size(new Size(51, 20));
			((Control)updnRadius).set_TabIndex(13);
			updnRadius.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnRadius).add_Enter((EventHandler)updnRadius_Enter);
			((Control)updnMoonAlt).set_Location(new Point(188, 107));
			updnMoonAlt.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnMoonAlt.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147483648 }));
			((Control)updnMoonAlt).set_Name("updnMoonAlt");
			((Control)updnMoonAlt).set_Size(new Size(37, 20));
			((Control)updnMoonAlt).set_TabIndex(22);
			((Control)updnMoonAlt).add_Enter((EventHandler)updnMoonAlt_Enter);
			((Control)updnIllum).set_Location(new Point(108, 107));
			((Control)updnIllum).set_Name("updnIllum");
			((Control)updnIllum).set_Size(new Size(41, 20));
			((Control)updnIllum).set_TabIndex(20);
			updnIllum.set_Value(new decimal(new int[4] { 100, 0, 0, 0 }));
			((Control)updnIllum).add_Enter((EventHandler)updnIllum_Enter);
			((Control)updnCA).set_Location(new Point(9, 106));
			updnCA.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnCA.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnCA).set_Name("updnCA");
			((Control)updnCA).set_Size(new Size(38, 20));
			((Control)updnCA).set_TabIndex(17);
			((Control)updnCA).add_Enter((EventHandler)updnCA_Enter);
			updnPA.set_DecimalPlaces(2);
			((Control)updnPA).set_Location(new Point(250, 70));
			updnPA.set_Maximum(new decimal(new int[4] { 35999, 0, 0, 131072 }));
			((Control)updnPA).set_Name("updnPA");
			((Control)updnPA).set_Size(new Size(61, 20));
			((Control)updnPA).set_TabIndex(15);
			((Control)updnPA).add_Enter((EventHandler)updnPA_Enter);
			updnLimbMotion.set_DecimalPlaces(4);
			updnLimbMotion.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnLimbMotion).set_Location(new Point(9, 69));
			updnLimbMotion.set_Maximum(new decimal(new int[4] { 8, 0, 0, 65536 }));
			((Control)updnLimbMotion).set_Name("updnLimbMotion");
			((Control)updnLimbMotion).set_Size(new Size(58, 20));
			((Control)updnLimbMotion).set_TabIndex(9);
			((Control)updnLimbMotion).add_Enter((EventHandler)updnLimbMotion_Enter);
			updnAA.set_DecimalPlaces(3);
			((Control)updnAA).set_Location(new Point(9, 30));
			updnAA.set_Maximum(new decimal(new int[4] { 359999, 0, 0, 196608 }));
			((Control)updnAA).set_Name("updnAA");
			((Control)updnAA).set_Size(new Size(61, 20));
			((Control)updnAA).set_TabIndex(1);
			((Control)updnAA).add_Enter((EventHandler)updnAA_Enter);
			((Control)optMoon).set_AutoSize(true);
			((Control)optMoon).set_Location(new Point(8, 38));
			((Control)optMoon).set_Name("optMoon");
			((Control)optMoon).set_Size(new Size(52, 17));
			((Control)optMoon).set_TabIndex(1);
			((Control)optMoon).set_Text("Moon");
			((ButtonBase)optMoon).set_UseVisualStyleBackColor(true);
			((Control)optAsteroid).set_AutoSize(true);
			optAsteroid.set_Checked(true);
			((Control)optAsteroid).set_Location(new Point(6, 15));
			((Control)optAsteroid).set_Name("optAsteroid");
			((Control)optAsteroid).set_Size(new Size(63, 17));
			((Control)optAsteroid).set_TabIndex(0);
			optAsteroid.set_TabStop(true);
			((Control)optAsteroid).set_Text("Asteroid");
			((ButtonBase)optAsteroid).set_UseVisualStyleBackColor(true);
			optAsteroid.add_CheckedChanged((EventHandler)optAsteroid_CheckedChanged);
			((Control)groupBox5).get_Controls().Add((Control)(object)optMoon);
			((Control)groupBox5).get_Controls().Add((Control)(object)optAsteroid);
			((Control)groupBox5).set_Location(new Point(178, 292));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(103, 69));
			((Control)groupBox5).set_TabIndex(6);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Occultation by...");
			((Control)chkExclude).set_AutoSize(true);
			((Control)chkExclude).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkExclude).set_Location(new Point(179, 375));
			((Control)chkExclude).set_Name("chkExclude");
			((Control)chkExclude).set_Size(new Size(121, 21));
			((Control)chkExclude).set_TabIndex(32);
			((Control)chkExclude).set_Text("Tag to Exclude");
			((ButtonBase)chkExclude).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateSubmitted).set_BackColor(Color.Aquamarine);
			((Control)cmdUpdateSubmitted).set_ForeColor(Color.Brown);
			((Control)cmdUpdateSubmitted).set_Location(new Point(331, 398));
			((Control)cmdUpdateSubmitted).set_Name("cmdUpdateSubmitted");
			((Control)cmdUpdateSubmitted).set_Size(new Size(146, 52));
			((Control)cmdUpdateSubmitted).set_TabIndex(10);
			((Control)cmdUpdateSubmitted).set_Text("Submitted light curves:\r\nUpdate the submitted file");
			((ButtonBase)cmdUpdateSubmitted).set_UseVisualStyleBackColor(false);
			((Control)cmdUpdateSubmitted).add_Click((EventHandler)cmdUpdateSubmitted_Click);
			((Control)cmdAcceptChanges).set_BackColor(Color.Aquamarine);
			((Control)cmdAcceptChanges).set_ForeColor(Color.Brown);
			((Control)cmdAcceptChanges).set_Location(new Point(171, 398));
			((Control)cmdAcceptChanges).set_Name("cmdAcceptChanges");
			((Control)cmdAcceptChanges).set_Size(new Size(132, 52));
			((Control)cmdAcceptChanges).set_TabIndex(9);
			((Control)cmdAcceptChanges).set_Text("Main light curves:\r\nAccept the revised data.\r\nFile changed on Exit");
			((ButtonBase)cmdAcceptChanges).set_UseVisualStyleBackColor(false);
			((Control)cmdAcceptChanges).add_Click((EventHandler)cmdUpdateInMemory_Click);
			((Control)lblSubmittedAdvice).set_AutoSize(true);
			((Control)lblSubmittedAdvice).set_ForeColor(Color.Red);
			((Control)lblSubmittedAdvice).set_Location(new Point(331, 450));
			((Control)lblSubmittedAdvice).set_Name("lblSubmittedAdvice");
			((Control)lblSubmittedAdvice).set_Size(new Size(146, 26));
			((Control)lblSubmittedAdvice).set_TabIndex(12);
			((Control)lblSubmittedAdvice).set_Text("Click this to accept the edits. \r\nThis will update the file now.");
			((Control)lblMainAdvice).set_AutoSize(true);
			((Control)lblMainAdvice).set_ForeColor(Color.Red);
			((Control)lblMainAdvice).set_Location(new Point(171, 450));
			((Control)lblMainAdvice).set_Name("lblMainAdvice");
			((Control)lblMainAdvice).set_Size(new Size(154, 39));
			((Control)lblMainAdvice).set_TabIndex(11);
			((Control)lblMainAdvice).set_Text("Click this to accept the edits.\r\nMemory data will be updated.\r\nThe file will be updated on Exit.");
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(5000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_IsBalloon(true);
			toolTip1.set_ReshowDelay(20);
			((Control)picLightCurve).set_Location(new Point(0, 0));
			((Control)picLightCurve).set_Name("picLightCurve");
			((Control)picLightCurve).set_Size(new Size(661, 203));
			picLightCurve.set_TabIndex(29);
			picLightCurve.set_TabStop(false);
			toolTip1.SetToolTip((Control)(object)picLightCurve, "X");
			((Control)picLightCurve).add_MouseClick(new MouseEventHandler(picLightCurve_MouseClick));
			((Control)picLightCurve).add_MouseEnter((EventHandler)picLightCurve_MouseEnter);
			((Control)picLightCurve).add_MouseMove(new MouseEventHandler(picLightCurve_MouseMove));
			picCheck.set_Image((Image)Resources.check);
			((Control)picCheck).set_Location(new Point(307, 414));
			((Control)picCheck).set_Name("picCheck");
			((Control)picCheck).set_Size(new Size(20, 20));
			picCheck.set_SizeMode((PictureBoxSizeMode)4);
			picCheck.set_TabIndex(33);
			picCheck.set_TabStop(false);
			((Control)picCheck).set_Visible(false);
			((ScrollableControl)PanelPlot).set_AutoScroll(true);
			PanelPlot.set_BorderStyle((BorderStyle)2);
			((Control)PanelPlot).get_Controls().Add((Control)(object)picLightCurve);
			((Control)PanelPlot).set_Location(new Point(169, 495));
			((Control)PanelPlot).set_Name("PanelPlot");
			((Control)PanelPlot).set_Size(new Size(666, 227));
			((Control)PanelPlot).set_TabIndex(34);
			((ScrollableControl)PanelPlot).add_Scroll(new ScrollEventHandler(PanelPlot_Scroll));
			((Control)panel3).set_BackColor(Color.Honeydew);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x16);
			((Control)panel3).get_Controls().Add((Control)(object)chkShowSkippedFrames);
			((Control)panel3).get_Controls().Add((Control)(object)chkTimeHeight);
			((Control)panel3).get_Controls().Add((Control)(object)label37);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x8);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x4);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x2);
			((Control)panel3).get_Controls().Add((Control)(object)cmd_x1);
			((Control)panel3).get_Controls().Add((Control)(object)updnScale);
			((Control)panel3).set_Location(new Point(1, 600));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(162, 119));
			((Control)panel3).set_TabIndex(35);
			((Control)cmd_x16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x16).set_Location(new Point(119, 48));
			((Control)cmd_x16).set_Name("cmd_x16");
			((Control)cmd_x16).set_Size(new Size(28, 20));
			((Control)cmd_x16).set_TabIndex(39);
			((Control)cmd_x16).set_Text("16");
			((ButtonBase)cmd_x16).set_UseVisualStyleBackColor(true);
			((Control)cmd_x16).add_Click((EventHandler)cmd_x16_Click);
			((Control)chkShowSkippedFrames).set_AutoSize(true);
			((Control)chkShowSkippedFrames).set_Location(new Point(11, 97));
			((Control)chkShowSkippedFrames).set_Name("chkShowSkippedFrames");
			((Control)chkShowSkippedFrames).set_Size(new Size(127, 17));
			((Control)chkShowSkippedFrames).set_TabIndex(38);
			((Control)chkShowSkippedFrames).set_Text("Show skipped frames");
			((ButtonBase)chkShowSkippedFrames).set_UseVisualStyleBackColor(true);
			((Control)chkShowSkippedFrames).add_Click((EventHandler)chkShowSkippedFrames_Click);
			((Control)chkTimeHeight).set_AutoSize(true);
			chkTimeHeight.set_Checked(true);
			chkTimeHeight.set_CheckState((CheckState)1);
			((Control)chkTimeHeight).set_Location(new Point(11, 76));
			((Control)chkTimeHeight).set_Name("chkTimeHeight");
			((Control)chkTimeHeight).set_Size(new Size(122, 17));
			((Control)chkTimeHeight).set_TabIndex(37);
			((Control)chkTimeHeight).set_Text("Show Time && Height");
			((ButtonBase)chkTimeHeight).set_UseVisualStyleBackColor(true);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(15, 3));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(123, 13));
			((Control)label37).set_TabIndex(36);
			((Control)label37).set_Text("Horizontal plot scale");
			((Control)cmd_x8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x8).set_Location(new Point(92, 48));
			((Control)cmd_x8).set_Name("cmd_x8");
			((Control)cmd_x8).set_Size(new Size(15, 20));
			((Control)cmd_x8).set_TabIndex(5);
			((Control)cmd_x8).set_Text("8");
			((ButtonBase)cmd_x8).set_UseVisualStyleBackColor(true);
			((Control)cmd_x8).add_Click((EventHandler)cmd_x8_Click);
			((Control)cmd_x4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x4).set_Location(new Point(65, 48));
			((Control)cmd_x4).set_Name("cmd_x4");
			((Control)cmd_x4).set_Size(new Size(15, 20));
			((Control)cmd_x4).set_TabIndex(4);
			((Control)cmd_x4).set_Text("4");
			((ButtonBase)cmd_x4).set_UseVisualStyleBackColor(true);
			((Control)cmd_x4).add_Click((EventHandler)cmd_x4_Click);
			((Control)cmd_x2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x2).set_Location(new Point(38, 48));
			((Control)cmd_x2).set_Name("cmd_x2");
			((Control)cmd_x2).set_Size(new Size(15, 20));
			((Control)cmd_x2).set_TabIndex(3);
			((Control)cmd_x2).set_Text("2");
			((ButtonBase)cmd_x2).set_UseVisualStyleBackColor(true);
			((Control)cmd_x2).add_Click((EventHandler)cmd_x2_Click);
			((Control)cmd_x1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd_x1).set_Location(new Point(11, 48));
			((Control)cmd_x1).set_Name("cmd_x1");
			((Control)cmd_x1).set_Size(new Size(15, 20));
			((Control)cmd_x1).set_TabIndex(2);
			((Control)cmd_x1).set_Text("1");
			((ButtonBase)cmd_x1).set_UseVisualStyleBackColor(true);
			((Control)cmd_x1).add_Click((EventHandler)cmd_x1_Click);
			updnScale.set_DecimalPlaces(2);
			((Control)updnScale).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnScale.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnScale).set_Location(new Point(58, 22));
			updnScale.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnScale.set_Minimum(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnScale).set_Name("updnScale");
			((Control)updnScale).set_Size(new Size(42, 20));
			((Control)updnScale).set_TabIndex(1);
			updnScale.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnScale.add_ValueChanged((EventHandler)updnScale_ValueChanged);
			((Control)chkValid).set_AutoSize(true);
			((Control)chkValid).set_Location(new Point(6, 6));
			((Control)chkValid).set_Name("chkValid");
			((Control)chkValid).set_Size(new Size(146, 17));
			((Control)chkValid).set_TabIndex(37);
			((Control)chkValid).set_Text("Check for skipped frames");
			((ButtonBase)chkValid).set_UseVisualStyleBackColor(true);
			chkValid.add_CheckedChanged((EventHandler)chkValid_CheckedChanged);
			((Control)panel2).set_BackColor(Color.PaleTurquoise);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)chkValid);
			((Control)panel2).get_Controls().Add((Control)(object)cmdZerovalues);
			((Control)panel2).set_Location(new Point(1, 522));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(162, 66));
			((Control)panel2).set_TabIndex(38);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Location(new Point(14, 7));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(129, 13));
			((Control)label49).set_TabIndex(18);
			((Control)label49).set_Text("Edit by light curve number");
			((Control)panel4).set_BackColor(Color.Thistle);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)label49);
			((Control)panel4).get_Controls().Add((Control)(object)updnLightCurveNumber);
			((Control)panel4).get_Controls().Add((Control)(object)cmdEdit);
			((Control)panel4).set_Location(new Point(1, 447));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(162, 63));
			((Control)panel4).set_TabIndex(39);
			((Control)cmdHelp).set_BackColor(Color.FromArgb(192, 255, 255));
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(179, 39));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(58, 27));
			((Control)cmdHelp).set_TabIndex(40);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)chkReview).set_AutoSize(true);
			((Control)chkReview).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkReview).set_Location(new Point(334, 375));
			((Control)chkReview).set_Name("chkReview");
			((Control)chkReview).set_Size(new Size(122, 21));
			((Control)chkReview).set_TabIndex(41);
			((Control)chkReview).set_Text("Tag for Review");
			((ButtonBase)chkReview).set_UseVisualStyleBackColor(true);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyLightCurveToolStripMenuItem,
				(ToolStripItem)lightCurveEditingToolStripMenuItem,
				(ToolStripItem)displayEventInAOTAToolStripMenuItem,
				(ToolStripItem)exitSaveChangesToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(843, 24));
			((Control)menuStrip1).set_TabIndex(44);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Name("copyLightCurveToolStripMenuItem");
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Size(new Size(158, 20));
			((ToolStripItem)copyLightCurveToolStripMenuItem).set_Text("Copy light curve image");
			((ToolStripItem)copyLightCurveToolStripMenuItem).add_Click((EventHandler)copyLightCurveToolStripMenuItem_Click);
			((ToolStripDropDownItem)lightCurveEditingToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)integrationToolStripMenuItem,
				(ToolStripItem)truncationToolStripMenuItem,
				(ToolStripItem)invalidFramesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)siteEditsToolStripMenuItem,
				(ToolStripItem)swapLatitudeLongitudeValuesToolStripMenuItem,
				(ToolStripItem)flipSignOfLongitudeToolStripMenuItem,
				(ToolStripItem)flipSignOfLatitudeToolStripMenuItem
			});
			((ToolStripItem)lightCurveEditingToolStripMenuItem).set_Name("lightCurveEditingToolStripMenuItem");
			((ToolStripItem)lightCurveEditingToolStripMenuItem).set_Size(new Size(159, 20));
			((ToolStripItem)lightCurveEditingToolStripMenuItem).set_Text("Light Curve - specific edits");
			((ToolStripDropDownItem)integrationToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)binIntegrationblocksToolStripMenuItem1,
				(ToolStripItem)undoIntegrationToolStripMenuItem
			});
			((ToolStripItem)integrationToolStripMenuItem).set_Name("integrationToolStripMenuItem");
			((ToolStripItem)integrationToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)integrationToolStripMenuItem).set_Text("Integration");
			((ToolStripItem)integrationToolStripMenuItem).add_Click((EventHandler)integrationToolStripMenuItem_Click);
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Name("binIntegrationblocksToolStripMenuItem1");
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Size(new Size(189, 22));
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).set_Text("Bin integration blocks");
			((ToolStripItem)binIntegrationblocksToolStripMenuItem1).add_Click((EventHandler)binIntegrationblocksToolStripMenuItem1_Click);
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Name("undoIntegrationToolStripMenuItem");
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Size(new Size(189, 22));
			((ToolStripItem)undoIntegrationToolStripMenuItem).set_Text("Undo binning");
			((ToolStripItem)undoIntegrationToolStripMenuItem).add_Click((EventHandler)undoIntegrationToolStripMenuItem_Click);
			((ToolStripDropDownItem)truncationToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)truncateToolStripMenuItem,
				(ToolStripItem)undoTruncationToolStripMenuItem
			});
			((ToolStripItem)truncationToolStripMenuItem).set_Name("truncationToolStripMenuItem");
			((ToolStripItem)truncationToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)truncationToolStripMenuItem).set_Text("Truncation");
			((ToolStripItem)truncateToolStripMenuItem).set_Name("truncateToolStripMenuItem");
			((ToolStripItem)truncateToolStripMenuItem).set_Size(new Size(161, 22));
			((ToolStripItem)truncateToolStripMenuItem).set_Text("Truncate");
			((ToolStripItem)truncateToolStripMenuItem).add_Click((EventHandler)truncateToolStripMenuItem_Click);
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Name("undoTruncationToolStripMenuItem");
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Size(new Size(161, 22));
			((ToolStripItem)undoTruncationToolStripMenuItem).set_Text("Undo truncation");
			((ToolStripItem)undoTruncationToolStripMenuItem).add_Click((EventHandler)undoTruncationToolStripMenuItem_Click);
			((ToolStripItem)invalidFramesToolStripMenuItem).set_Name("invalidFramesToolStripMenuItem");
			((ToolStripItem)invalidFramesToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)invalidFramesToolStripMenuItem).set_Text("Invalid frame editor - make visible");
			((ToolStripItem)invalidFramesToolStripMenuItem).add_Click((EventHandler)invalidFramesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(251, 6));
			((ToolStripItem)siteEditsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)siteEditsToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)siteEditsToolStripMenuItem).set_Name("siteEditsToolStripMenuItem");
			((ToolStripItem)siteEditsToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)siteEditsToolStripMenuItem).set_Text("Site edits");
			((ToolStripItem)swapLatitudeLongitudeValuesToolStripMenuItem).set_Name("swapLatitudeLongitudeValuesToolStripMenuItem");
			((ToolStripItem)swapLatitudeLongitudeValuesToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)swapLatitudeLongitudeValuesToolStripMenuItem).set_Text("*  Swap Latitude/Longitude values");
			((ToolStripItem)swapLatitudeLongitudeValuesToolStripMenuItem).add_Click((EventHandler)swapLatitudeLongitudeValuesToolStripMenuItem_Click);
			((ToolStripItem)flipSignOfLongitudeToolStripMenuItem).set_Name("flipSignOfLongitudeToolStripMenuItem");
			((ToolStripItem)flipSignOfLongitudeToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)flipSignOfLongitudeToolStripMenuItem).set_Text("*  Flip sign of longitude");
			((ToolStripItem)flipSignOfLongitudeToolStripMenuItem).add_Click((EventHandler)flipSignOfLongitudeToolStripMenuItem_Click);
			((ToolStripItem)flipSignOfLatitudeToolStripMenuItem).set_Name("flipSignOfLatitudeToolStripMenuItem");
			((ToolStripItem)flipSignOfLatitudeToolStripMenuItem).set_Size(new Size(254, 22));
			((ToolStripItem)flipSignOfLatitudeToolStripMenuItem).set_Text("*  Flip sign of Latitude");
			((ToolStripItem)flipSignOfLatitudeToolStripMenuItem).add_Click((EventHandler)flipSignOfLatitudeToolStripMenuItem_Click);
			((ToolStripItem)displayEventInAOTAToolStripMenuItem).set_Name("displayEventInAOTAToolStripMenuItem");
			((ToolStripItem)displayEventInAOTAToolStripMenuItem).set_Size(new Size(146, 20));
			((ToolStripItem)displayEventInAOTAToolStripMenuItem).set_Text("Display event in AOTA    ");
			((ToolStripItem)displayEventInAOTAToolStripMenuItem).add_Click((EventHandler)displayEventInAOTAToolStripMenuItem_Click);
			((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Name("exitSaveChangesToolStripMenuItem");
			((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Size(new Size(140, 20));
			((ToolStripItem)exitSaveChangesToolStripMenuItem).set_Text("Exit && save changes");
			((ToolStripItem)exitSaveChangesToolStripMenuItem).add_Click((EventHandler)exitSaveChangesToolStripMenuItem_Click);
			((Control)pnlIntegration).set_BackColor(Color.MintCream);
			pnlIntegration.set_BorderStyle((BorderStyle)2);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)panel6);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)label59);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)cmdIntegrate);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)cmdCancel);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)label58);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)updnFramesToCombine);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)label57);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)updnFirst);
			((Control)pnlIntegration).get_Controls().Add((Control)(object)label56);
			((Control)pnlIntegration).set_Location(new Point(563, 51));
			((Control)pnlIntegration).set_Name("pnlIntegration");
			((Control)pnlIntegration).set_Size(new Size(261, 362));
			((Control)pnlIntegration).set_TabIndex(45);
			((Control)pnlIntegration).set_Visible(false);
			((Control)panel6).set_BackColor(Color.Lavender);
			panel6.set_BorderStyle((BorderStyle)2);
			((Control)panel6).get_Controls().Add((Control)(object)label62);
			((Control)panel6).get_Controls().Add((Control)(object)label60);
			((Control)panel6).get_Controls().Add((Control)(object)label61);
			((Control)panel6).set_Location(new Point(1, 136));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(254, 222));
			((Control)panel6).set_TabIndex(11);
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Location(new Point(2, 126));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(239, 91));
			((Control)label62).set_TabIndex(10);
			((Control)label62).set_Text(componentResourceManager.GetString("label62.Text"));
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label60).set_ForeColor(Color.Red);
			((Control)label60).set_Location(new Point(41, 2));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(169, 13));
			((Control)label60).set_TabIndex(8);
			((Control)label60).set_Text("V E R Y   I M P O R T A N T");
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Location(new Point(0, 16));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(254, 104));
			((Control)label61).set_TabIndex(9);
			((Control)label61).set_Text(componentResourceManager.GetString("label61.Text"));
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_ForeColor(Color.Maroon);
			((Control)label59).set_Location(new Point(16, 21));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(224, 26));
			((Control)label59).set_TabIndex(7);
			((Control)label59).set_Text("Change Start frame, and  #Frames to combine\r\nso that the lines match the integration steps");
			((Control)cmdIntegrate).set_BackColor(Color.LightGreen);
			((Control)cmdIntegrate).set_Location(new Point(29, 100));
			((Control)cmdIntegrate).set_Name("cmdIntegrate");
			((Control)cmdIntegrate).set_Size(new Size(101, 25));
			((Control)cmdIntegrate).set_TabIndex(6);
			((Control)cmdIntegrate).set_Text("Update && close");
			((ButtonBase)cmdIntegrate).set_UseVisualStyleBackColor(false);
			((Control)cmdIntegrate).add_Click((EventHandler)cmdIntegrate_Click);
			((Control)cmdCancel).set_BackColor(Color.MistyRose);
			((Control)cmdCancel).set_Location(new Point(178, 100));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(49, 25));
			((Control)cmdCancel).set_TabIndex(5);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Location(new Point(117, 54));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(106, 13));
			((Control)label58).set_TabIndex(4);
			((Control)label58).set_Text("# Frames to combine");
			((Control)updnFramesToCombine).set_Location(new Point(149, 68));
			updnFramesToCombine.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnFramesToCombine.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFramesToCombine).set_Name("updnFramesToCombine");
			((Control)updnFramesToCombine).set_Size(new Size(42, 20));
			((Control)updnFramesToCombine).set_TabIndex(3);
			updnFramesToCombine.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnFramesToCombine.add_ValueChanged((EventHandler)updnFramesToCombine_ValueChanged);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Location(new Point(46, 54));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(58, 13));
			((Control)label57).set_TabIndex(2);
			((Control)label57).set_Text("Start frame");
			((Control)updnFirst).set_Location(new Point(54, 68));
			updnFirst.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnFirst).set_Name("updnFirst");
			((Control)updnFirst).set_Size(new Size(42, 20));
			((Control)updnFirst).set_TabIndex(1);
			updnFirst.add_ValueChanged((EventHandler)updnFirst_ValueChanged);
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(28, 3));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(200, 13));
			((Control)label56).set_TabIndex(0);
			((Control)label56).set_Text("Rectify light curves for integration");
			((Control)pnlTruncate).set_BackColor(Color.Cornsilk);
			pnlTruncate.set_BorderStyle((BorderStyle)2);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)panel7);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)cmdTruncate);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)cmdCancelTruncate);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)label65);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)updnLastFrame);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)label66);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)updnFirstFrame);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)label64);
			((Control)pnlTruncate).get_Controls().Add((Control)(object)label63);
			((Control)pnlTruncate).set_Location(new Point(746, 114));
			((Control)pnlTruncate).set_Name("pnlTruncate");
			((Control)pnlTruncate).set_Size(new Size(261, 362));
			((Control)pnlTruncate).set_TabIndex(46);
			((Control)pnlTruncate).set_Visible(false);
			((Control)panel7).set_BackColor(Color.Lavender);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)label67);
			((Control)panel7).get_Controls().Add((Control)(object)label68);
			((Control)panel7).get_Controls().Add((Control)(object)label69);
			((Control)panel7).set_Location(new Point(1, 134));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(254, 222));
			((Control)panel7).set_TabIndex(15);
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Location(new Point(2, 126));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(239, 91));
			((Control)label67).set_TabIndex(10);
			((Control)label67).set_Text(componentResourceManager.GetString("label67.Text"));
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label68).set_ForeColor(Color.Red);
			((Control)label68).set_Location(new Point(41, 2));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(169, 13));
			((Control)label68).set_TabIndex(8);
			((Control)label68).set_Text("V E R Y   I M P O R T A N T");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Location(new Point(0, 16));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(254, 104));
			((Control)label69).set_TabIndex(9);
			((Control)label69).set_Text(componentResourceManager.GetString("label69.Text"));
			((Control)cmdTruncate).set_BackColor(Color.LightGreen);
			((Control)cmdTruncate).set_Location(new Point(36, 96));
			((Control)cmdTruncate).set_Name("cmdTruncate");
			((Control)cmdTruncate).set_Size(new Size(101, 25));
			((Control)cmdTruncate).set_TabIndex(14);
			((Control)cmdTruncate).set_Text("Update && close");
			((ButtonBase)cmdTruncate).set_UseVisualStyleBackColor(false);
			((Control)cmdTruncate).add_Click((EventHandler)cmdTruncate_Click);
			((Control)cmdCancelTruncate).set_BackColor(Color.MistyRose);
			((Control)cmdCancelTruncate).set_Location(new Point(171, 96));
			((Control)cmdCancelTruncate).set_Name("cmdCancelTruncate");
			((Control)cmdCancelTruncate).set_Size(new Size(49, 25));
			((Control)cmdCancelTruncate).set_TabIndex(13);
			((Control)cmdCancelTruncate).set_Text("Cancel");
			((ButtonBase)cmdCancelTruncate).set_UseVisualStyleBackColor(false);
			((Control)cmdCancelTruncate).add_Click((EventHandler)cmdCancelTruncate_Click);
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Location(new Point(147, 50));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(56, 13));
			((Control)label65).set_TabIndex(12);
			((Control)label65).set_Text("Last frame");
			((Control)updnLastFrame).set_Location(new Point(151, 64));
			updnLastFrame.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnLastFrame.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLastFrame).set_Name("updnLastFrame");
			((Control)updnLastFrame).set_Size(new Size(49, 20));
			((Control)updnLastFrame).set_TabIndex(11);
			updnLastFrame.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnLastFrame.add_ValueChanged((EventHandler)updnLastFrame_ValueChanged);
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Location(new Point(53, 50));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(55, 13));
			((Control)label66).set_TabIndex(10);
			((Control)label66).set_Text("First frame");
			((Control)updnFirstFrame).set_Location(new Point(56, 64));
			updnFirstFrame.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnFirstFrame).set_Name("updnFirstFrame");
			((Control)updnFirstFrame).set_Size(new Size(49, 20));
			((Control)updnFirstFrame).set_TabIndex(9);
			updnFirstFrame.add_ValueChanged((EventHandler)updnFirstFrame_ValueChanged);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_ForeColor(Color.Maroon);
			((Control)label64).set_Location(new Point(14, 20));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(229, 26));
			((Control)label64).set_TabIndex(8);
			((Control)label64).set_Text("Change First frame, and  Last frames to set the \r\nbeginning and end of the light curve");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(40, 3));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(122, 13));
			((Control)label63).set_TabIndex(1);
			((Control)label63).set_Text("Truncate light curve");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(843, 726));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)panelSetInvalid);
			((Control)this).get_Controls().Add((Control)(object)pnlTruncate);
			((Control)this).get_Controls().Add((Control)(object)pnlIntegration);
			((Control)this).get_Controls().Add((Control)(object)chkReview);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)PanelPlot);
			((Control)this).get_Controls().Add((Control)(object)picCheck);
			((Control)this).get_Controls().Add((Control)(object)lblMainUpdated);
			((Control)this).get_Controls().Add((Control)(object)lblMainAdvice);
			((Control)this).get_Controls().Add((Control)(object)lblSubmittedAdvice);
			((Control)this).get_Controls().Add((Control)(object)cmdAcceptChanges);
			((Control)this).get_Controls().Add((Control)(object)cmdUpdateSubmitted);
			((Control)this).get_Controls().Add((Control)(object)chkExclude);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)grpMoon);
			((Control)this).get_Controls().Add((Control)(object)groupAsteroid);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)lblSource);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("LightCurve_Edit");
			((Control)this).set_Text("Light Curve Editor");
			((Form)this).add_FormClosing(new FormClosingEventHandler(LightCurve_Edit_FormClosing));
			((Form)this).add_Load((EventHandler)LightCurve_Edit_Load);
			((ISupportInitialize)updnLightCurveNumber).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnSecond).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((Control)panelSetInvalid).ResumeLayout(false);
			((Control)panelSetInvalid).PerformLayout();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((ISupportInitialize)updnPoints).EndInit();
			((ISupportInitialize)updnDuration).EndInit();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)updnAlt).EndInit();
			((ISupportInitialize)updnLatSec).EndInit();
			((ISupportInitialize)updnLatDeg).EndInit();
			((ISupportInitialize)updnLatMin).EndInit();
			((ISupportInitialize)updnLongSec).EndInit();
			((ISupportInitialize)updnLongDeg).EndInit();
			((ISupportInitialize)updnLongMin).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((ISupportInitialize)updnTb).EndInit();
			((ISupportInitialize)updnU4b).EndInit();
			((ISupportInitialize)updnTa).EndInit();
			((ISupportInitialize)updnTc).EndInit();
			((ISupportInitialize)updnU4a).EndInit();
			((ISupportInitialize)updnSAO).EndInit();
			((ISupportInitialize)updnXZ).EndInit();
			((ISupportInitialize)updnZC).EndInit();
			((ISupportInitialize)updnHip).EndInit();
			((Control)groupAsteroid).ResumeLayout(false);
			((Control)groupAsteroid).PerformLayout();
			((ISupportInitialize)updnAsteroidNo).EndInit();
			((Control)grpMoon).ResumeLayout(false);
			((Control)grpMoon).PerformLayout();
			((ISupportInitialize)updnLibL).EndInit();
			((ISupportInitialize)updnLibB).EndInit();
			((ISupportInitialize)updnSlope).EndInit();
			((ISupportInitialize)updnContact).EndInit();
			((ISupportInitialize)updnRadius).EndInit();
			((ISupportInitialize)updnMoonAlt).EndInit();
			((ISupportInitialize)updnIllum).EndInit();
			((ISupportInitialize)updnCA).EndInit();
			((ISupportInitialize)updnPA).EndInit();
			((ISupportInitialize)updnLimbMotion).EndInit();
			((ISupportInitialize)updnAA).EndInit();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((ISupportInitialize)picLightCurve).EndInit();
			((ISupportInitialize)picCheck).EndInit();
			((Control)PanelPlot).ResumeLayout(false);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnScale).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)pnlIntegration).ResumeLayout(false);
			((Control)pnlIntegration).PerformLayout();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((ISupportInitialize)updnFramesToCombine).EndInit();
			((ISupportInitialize)updnFirst).EndInit();
			((Control)pnlTruncate).ResumeLayout(false);
			((Control)pnlTruncate).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((ISupportInitialize)updnLastFrame).EndInit();
			((ISupportInitialize)updnFirstFrame).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
