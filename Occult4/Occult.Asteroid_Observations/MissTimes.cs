using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class MissTimes : Form
	{
		internal int[] Rec = new int[200];

		internal double[] Tcorrn = new double[200];

		private IContainer components;

		private Button cmdCorrect;

		internal ListBox lstMiss;

		private Button cmdHelp;

		public MissTimes()
		{
			InitializeComponent();
		}

		internal void MClose()
		{
			((Form)this).Close();
		}

		private void cmdCorrect_Click(object sender, EventArgs e)
		{
			CorrectTimes();
		}

		internal void CorrectTimes()
		{
			for (int i = 0; i < 5; i++)
			{
				if (lstMiss.get_Items().get_Count() > 0)
				{
					for (int j = 0; j < lstMiss.get_Items().get_Count(); j++)
					{
						EventDetails.Observers[Rec[j]].UpdateMissTime(Tcorrn[j]);
					}
					Data_and_Plots.Observations_Editor.PopulateListOfObservers(0);
					Data_and_Plots.Observations_Editor.SetEditDate();
				}
				((Control)Data_and_Plots.PlotForm).Focus();
				Data_and_Plots.ClosestMissTimes();
			}
			((Form)this).Close();
		}

		private void MissTimes_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Closest Miss times");
		}

		private void MissTimes_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
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
			//IL_022a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0234: Expected O, but got Unknown
			//IL_0293: Unknown result type (might be due to invalid IL or missing references)
			//IL_029d: Expected O, but got Unknown
			cmdCorrect = new Button();
			lstMiss = new ListBox();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)cmdCorrect).set_Location(new Point(116, 114));
			((Control)cmdCorrect).set_Name("cmdCorrect");
			((Control)cmdCorrect).set_Size(new Size(138, 27));
			((Control)cmdCorrect).set_TabIndex(0);
			((Control)cmdCorrect).set_Text("Correct times in report");
			((ButtonBase)cmdCorrect).set_UseVisualStyleBackColor(true);
			((Control)cmdCorrect).add_Click((EventHandler)cmdCorrect_Click);
			lstMiss.set_ColumnWidth(120);
			((Control)lstMiss).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMiss).set_FormattingEnabled(true);
			lstMiss.set_ItemHeight(14);
			((Control)lstMiss).set_Location(new Point(7, 4));
			lstMiss.set_MultiColumn(true);
			((Control)lstMiss).set_Name("lstMiss");
			((Control)lstMiss).set_Size(new Size(360, 102));
			((Control)lstMiss).set_TabIndex(1);
			((Control)cmdHelp).set_Location(new Point(308, 117));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(39, 21));
			((Control)cmdHelp).set_TabIndex(2);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(371, 147));
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)lstMiss);
			((Control)this).get_Controls().Add((Control)(object)cmdCorrect);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterMissTimes", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)5);
			((Form)this).set_Location(Settings.Default.LocationAsterMissTimes);
			((Control)this).set_Name("MissTimes");
			((Control)this).set_Text("Adjustment - closest Miss times");
			((Form)this).add_Load((EventHandler)MissTimes_Load);
			((Form)this).add_FormClosed(new FormClosedEventHandler(MissTimes_FormClosed));
			((Control)this).ResumeLayout(false);
		}
	}
}
