using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class DoubleStarDeletions : Form
	{
		internal CheckBox[] DeleteSolution = (CheckBox[])(object)new CheckBox[14];

		private IContainer components;

		private Button cmdDelete;

		private Label label1;

		internal Label lblEvent;

		public DoubleStarDeletions()
		{
			InitializeComponent();
		}

		private void DoubleStarDeletions_Load(object sender, EventArgs e)
		{
			SetDoubleLines();
		}

		private void SetDoubleLines()
		{
			//IL_0037: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			for (int i = 0; i < 4; i++)
			{
				if (DeleteSolution[i] != null)
				{
					((Control)this).get_Controls().Remove((Control)(object)DeleteSolution[i]);
				}
			}
			for (int j = 0; j < 4; j++)
			{
				DeleteSolution[j] = new CheckBox();
				((Control)DeleteSolution[j]).set_Top(70 + 21 * j);
				((Control)DeleteSolution[j]).set_Left(10);
				((Control)DeleteSolution[j]).set_Width(700);
				((Control)DeleteSolution[j]).set_Text(" " + EventDetails.Doubles[j].ToString().Replace("-", ">"));
				((Control)DeleteSolution[j]).set_Font(new Font("Courier New", 8.25f));
				DeleteSolution[j].set_CheckAlign(ContentAlignment.MiddleLeft);
				((ButtonBase)DeleteSolution[j]).set_TextAlign(ContentAlignment.MiddleLeft);
				((Control)DeleteSolution[j]).set_Visible(true);
				((Control)DeleteSolution[j]).set_Enabled(EventDetails.Doubles[j].Sep_Companion != 0.0);
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
			for (int num = EventDetails.Doubles.Count - 1; num >= 0; num--)
			{
				if (num != 4 && DeleteSolution[num].get_Checked())
				{
					DoubleData doubleData = EventDetails.Doubles[num];
					DoubleData doubleData2 = EventDetails.Doubles[num];
					DoubleData doubleData3 = EventDetails.Doubles[num];
					double num3 = (EventDetails.Doubles[num].Sdev_Sep_Companion = 0.0);
					double num5 = (doubleData3.Sdev_PA_Companion = num3);
					double num8 = (doubleData.Sep_Companion = (doubleData2.PA_Companion = num5));
				}
			}
			EventDetails.StarIsDouble = false;
			for (int i = 0; i < 4; i++)
			{
				if (EventDetails.Doubles[i].Sep_Companion > 0.0)
				{
					EventDetails.StarIsDouble = true;
				}
			}
			SetDoubleLines();
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
			cmdDelete = new Button();
			label1 = new Label();
			lblEvent = new Label();
			((Control)this).SuspendLayout();
			((Control)cmdDelete).set_Location(new Point(27, 8));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(78, 31));
			((Control)cmdDelete).set_TabIndex(0);
			((Control)cmdDelete).set_Text("Delete");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(17, 57));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(231, 14));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text(" Soln   Sep (masec)      P.A.   ");
			((Control)lblEvent).set_AutoSize(true);
			((Control)lblEvent).set_Location(new Point(124, 17));
			((Control)lblEvent).set_Name("lblEvent");
			((Control)lblEvent).set_Size(new Size(35, 13));
			((Control)lblEvent).set_TabIndex(2);
			((Control)lblEvent).set_Text("Event");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(270, 178));
			((Control)this).get_Controls().Add((Control)(object)lblEvent);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("DoubleStarDeletions");
			((Control)this).set_Text("Double Star Deletions");
			((Form)this).add_Load((EventHandler)DoubleStarDeletions_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
