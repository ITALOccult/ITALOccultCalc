using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class SatelliteDeletions : Form
	{
		internal CheckBox[] DeleteSolution = (CheckBox[])(object)new CheckBox[14];

		private IContainer components;

		internal Label lblEvent;

		private Label label1;

		private Button cmdDelete;

		public SatelliteDeletions()
		{
			InitializeComponent();
		}

		private void SatelliteDeletions_Load(object sender, EventArgs e)
		{
			SetSatellitesLines();
		}

		private void SetSatellitesLines()
		{
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_003e: Expected O, but got Unknown
			for (int i = 0; i < 14; i++)
			{
				if (DeleteSolution[i] != null)
				{
					((Control)this).get_Controls().Remove((Control)(object)DeleteSolution[i]);
				}
			}
			for (int j = 0; j < 4; j++)
			{
				DeleteSolution[j] = new CheckBox();
				((Control)DeleteSolution[j]).set_Top(90 + 21 * j);
				((Control)DeleteSolution[j]).set_Left(10);
				((Control)DeleteSolution[j]).set_Width(700);
				((Control)DeleteSolution[j]).set_Text(" " + EventDetails.Satellites[j].ToString());
				((Control)DeleteSolution[j]).set_Font(new Font("Courier New", 8.25f));
				DeleteSolution[j].set_CheckAlign(ContentAlignment.MiddleLeft);
				((ButtonBase)DeleteSolution[j]).set_TextAlign(ContentAlignment.MiddleLeft);
				((Control)DeleteSolution[j]).set_Visible(true);
				((Control)DeleteSolution[j]).set_Enabled(j < EventDetails.NumberOfSatellites);
				((Control)this).get_Controls().Add((Control)(object)DeleteSolution[j]);
			}
			for (int k = EventDetails.Doubles.Count; k < 14; k++)
			{
				if (DeleteSolution[k] != null)
				{
					((Control)DeleteSolution[k]).set_Visible(false);
				}
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < EventDetails.NumberOfSatellites; i++)
			{
				if (DeleteSolution[i].get_Checked())
				{
					SatelliteData value = new SatelliteData();
					EventDetails.Satellites[i] = value;
				}
			}
			EventDetails.Satellites.Sort();
			EventDetails.AsteroidHasSatellite = false;
			EventDetails.NumberOfSatellites = 0;
			for (int j = 0; j < 4; j++)
			{
				if (EventDetails.Satellites[j].SatelliteSeparation > 0.0)
				{
					EventDetails.NumberOfSatellites++;
					EventDetails.AsteroidHasSatellite = true;
				}
			}
			SetSatellitesLines();
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
			lblEvent = new Label();
			label1 = new Label();
			cmdDelete = new Button();
			((Control)this).SuspendLayout();
			((Control)lblEvent).set_AutoSize(true);
			((Control)lblEvent).set_Location(new Point(142, 30));
			((Control)lblEvent).set_Name("lblEvent");
			((Control)lblEvent).set_Size(new Size(35, 13));
			((Control)lblEvent).set_TabIndex(5);
			((Control)lblEvent).set_Text("Event");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(35, 70));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(308, 14));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text(" Satellite         Sep (masec)      P.A.   ");
			((Control)cmdDelete).set_Location(new Point(45, 21));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(78, 31));
			((Control)cmdDelete).set_TabIndex(3);
			((Control)cmdDelete).set_Text("Delete");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(400, 211));
			((Control)this).get_Controls().Add((Control)(object)lblEvent);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("SatelliteDeletions");
			((Control)this).set_Text("SatelliteDeletions");
			((Form)this).add_Load((EventHandler)SatelliteDeletions_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
